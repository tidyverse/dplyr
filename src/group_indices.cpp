#include "pch.h"
#include <dplyr/main.h>

#include <tools/match.h>

#include <dplyr/white_list.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/Order.h>

#include <dplyr/Result/Count.h>

#include <dplyr/train.h>

#include <dplyr/bad.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
IntegerVector grouped_indices_grouped_df_impl(GroupedDataFrame gdf) {
  int n = gdf.nrows();
  IntegerVector res = no_init(n);
  int ngroups = gdf.ngroups();
  GroupedDataFrameIndexIterator it = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++it) {
    const SlicingIndex& index = *it;
    int n_index = index.size();
    for (int j = 0; j < n_index; j++) {
      res[ index[j] ] = i + 1;
    }
  }
  return res;
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp(GroupedDataFrame gdf) {
  return Count().process(gdf);
}

DataFrame build_index_cpp(DataFrame data) {
  SymbolVector vars(get_vars(data));
  const int nvars = vars.size();

  CharacterVector names = data.names();
  IntegerVector indx = vars.match_in_table(names);

  for (int i = 0; i < nvars; ++i) {
    int pos = indx[i];
    if (pos == NA_INTEGER) {
      bad_col(vars[i], "is unknown");
    }

    SEXP v = data[pos - 1];

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      bad_col(vars[i], "can't be used as a grouping variable because it's a {type}",
              _["type"] = get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);
  ChunkIndexMap map(visitors);

  train_push_back(map, data.nrows());

  DataFrame labels = DataFrameSubsetVisitors(data, vars).subset(map, "data.frame");
  int ngroups = labels.nrows();
  IntegerVector labels_order = OrderVisitors(labels).apply();

  labels = DataFrameSubsetVisitors(labels).subset(labels_order, "data.frame");

  List indices(ngroups);
  IntegerVector group_sizes = no_init(ngroups);
  int biggest_group = 0;

  ChunkIndexMap::const_iterator it = map.begin();
  std::vector<const std::vector<int>* > chunks(ngroups);
  for (int i = 0; i < ngroups; i++, ++it) {
    chunks[i] = &it->second;
  }

  for (int i = 0; i < ngroups; i++) {
    int idx = labels_order[i];
    const std::vector<int>& chunk = *chunks[idx];
    indices[i] = chunk;
    group_sizes[i] = chunk.size();
    biggest_group = std::max(biggest_group, (int)chunk.size());
  }

  data.attr("indices") = indices;
  data.attr("group_sizes") = group_sizes;
  data.attr("biggest_group_size") = biggest_group;
  data.attr("labels") = labels;
  set_class(data, CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame"));
  return data;
}

void strip_index(DataFrame x) {
  x.attr("indices") = R_NilValue;
  x.attr("group_sizes") = R_NilValue;
  x.attr("biggest_group_size") = R_NilValue;
  x.attr("labels") = R_NilValue;
}

SEXP strip_group_attributes(SEXP df) {
  Shield<SEXP> attribs(Rf_cons(dplyr::classes_not_grouped(), R_NilValue));
  SET_TAG(attribs, Rf_install("class"));

  SEXP p = ATTRIB(df);
  std::vector<SEXP> black_list(8);
  black_list[0] = Rf_install("indices");
  black_list[1] = Rf_install("vars");
  black_list[2] = Rf_install("index");
  black_list[3] = Rf_install("labels");
  black_list[4] = Rf_install("drop");
  black_list[5] = Rf_install("group_sizes");
  black_list[6] = Rf_install("biggest_group_size");
  black_list[7] = Rf_install("class");

  SEXP q = attribs;
  while (! Rf_isNull(p)) {
    SEXP tag = TAG(p);
    if (std::find(black_list.begin(), black_list.end(), tag) == black_list.end()) {
      Shield<SEXP> s(Rf_cons(CAR(p), R_NilValue));
      SETCDR(q, s);
      q = CDR(q);
      SET_TAG(q, tag);
    }

    p = CDR(p);
  }
  return attribs;
}
