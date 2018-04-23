#include "pch.h"
#include <dplyr/main.h>
#include <dplyr/white_list.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/DataFrameJoinVisitors.h>

#include <dplyr/Order.h>

#include <dplyr/Result/Count.h>

#include <dplyr/train.h>

#include <dplyr/bad.h>
#include <dplyr/tbl_cpp.h>

#include <tools/match.h>

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

DataFrame expand_labels(DataFrame labels) {
  int nc = labels.ncol();
  List uniques(nc);
  std::vector<int> sizes(nc);
  int total_size = 1;

  // doing this with R callbacks for now, might revisit later
  // if this becomes a performance problem
  for (int i = 0; i < nc; i++) {
    SEXP obj = labels[i];
    if (Rf_inherits(obj, "factor")) {
      SEXP labels = Rf_getAttrib(obj, R_LevelsSymbol);
      IntegerVector values = seq_len(Rf_length(labels));
      copy_attributes(values, obj);
      uniques[i] = values;
    } else {
      uniques[i] = Rcpp_eval(Language("unique", obj));
    }
    sizes[i] = Rf_length(uniques[i]);
    total_size *= sizes[i];
  }
  uniques.names() = labels.names();
  uniques.push_back(false, "stringsAsFactors");
  Language call("do.call", Symbol("expand.grid"), uniques);
  DataFrame new_labels = Rcpp_eval(call);
  // cleanup after expand.grid
  new_labels.attr("out.attrs") = R_NilValue;

  IntegerVector new_labels_order = OrderVisitors(new_labels).apply();
  return DataFrameSubsetVisitors(new_labels).subset(new_labels_order, "data.frame");

}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
void build_index_cpp(DataFrame& data, bool drop, bool expand) {
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

  // the labels that are effectively present in the data
  DataFrame labels = DataFrameSubsetVisitors(data, vars).subset(map, "data.frame");

  int ngroups = labels.nrows();
  IntegerVector labels_order = OrderVisitors(labels).apply();

  labels = DataFrameSubsetVisitors(labels).subset(labels_order, "data.frame");

  ChunkIndexMap::const_iterator it = map.begin();
  std::vector<const std::vector<int>* > chunks(ngroups);
  for (int i = 0; i < ngroups; i++, ++it) {
    chunks[i] = &it->second;
  }
  int biggest_group = 0;

  if (!drop) {
    // not dropping zero length groups
    // so we need to expand labels to contain all combinations
    DataFrame expanded_labels = expand_labels(labels) ;

    DataFrameJoinVisitors join_visitors(labels, expanded_labels, vars, vars, true, true);
    typedef VisitorSetIndexSet<DataFrameJoinVisitors> ChunkIndexJoinSet;
    ChunkIndexJoinSet join_set(join_visitors);

    // train the join set in terms of labels
    train_insert(join_set, labels.nrows());

    ngroups = expanded_labels.nrows() ;
    List indices(ngroups);
    IntegerVector group_sizes = no_init(ngroups);

    for (int i = 0; i < ngroups; i++) {
      // is the group in the row i of expanded labels in labels ?
      ChunkIndexJoinSet::iterator it = join_set.find(-i - 1);
      if (it == join_set.end()) {
        // did not find -> empty indices
        group_sizes[i] = 0 ;
        indices[i] = IntegerVector::create();
      } else {
        // the index from labels that corresponds to this row
        // of expanded_labels
        int idx = labels_order[*it] ;

        const std::vector<int>& chunk = *chunks[idx];
        indices[i] = chunk;
        group_sizes[i] = chunk.size();
        biggest_group = std::max(biggest_group, (int)chunk.size());
      }

    }

    data.attr("indices") = indices;
    data.attr("group_sizes") = group_sizes;
    data.attr("biggest_group_size") = biggest_group;
    data.attr("labels") = expanded_labels;


  } else {
    // drop zero length group - so just organise what was
    // collected by the ChunkIndexMap above

    List indices(ngroups);
    IntegerVector group_sizes = no_init(ngroups);

    for (int i = 0; i < ngroups; i++) {
      int idx = labels_order[i];
      const std::vector<int>& chunk = *chunks[idx];
      indices[i] = chunk;
      group_sizes[i] = chunk.size();
      biggest_group = std::max(biggest_group, (int)chunk.size());
    }

    // The attributes are injected into data without duplicating it!
    // The object is mutated, violating R's usual copy-on-write semantics.
    // This is safe here, because the indices are an auxiliary data structure
    // that is rebuilt as necessary. Updating the object in-place saves costly
    // recomputations. We don't touch the "class" attribute here.
    data.attr("indices") = indices;
    data.attr("group_sizes") = group_sizes;
    data.attr("biggest_group_size") = biggest_group;
    data.attr("labels") = labels;
  }

}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
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
  std::vector<SEXP> black_list(9);
  black_list[0] = Rf_install("indices");
  black_list[1] = Rf_install("vars");
  black_list[2] = Rf_install("index");
  black_list[3] = Rf_install("labels");
  black_list[4] = Rf_install("drop");
  black_list[5] = Rf_install("group_sizes");
  black_list[6] = Rf_install("biggest_group_size");
  black_list[7] = Rf_install("class");
  black_list[8] = Rf_install("expand");

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
