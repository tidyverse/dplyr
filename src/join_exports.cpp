#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/match.h>
#include <tools/Quosure.h>
#include <tools/set_rownames.h>
#include <tools/train.h>
#include <tools/bad.h>
#include <tools/debug.h>

#include <dplyr/data/GroupedDataFrame.h>

#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/visitors/join/DataFrameJoinVisitors.h>
#include <dplyr/visitors/subset/DataFrameSelect.h>
#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>

namespace dplyr {

Rcpp::DataFrame subset_join(Rcpp::DataFrame x, Rcpp::DataFrame y,
                            const std::vector<int>& indices_x, const std::vector<int>& indices_y,
                            const Rcpp::IntegerVector& by_x, const Rcpp::IntegerVector& by_y,
                            const Rcpp::IntegerVector& aux_x, const Rcpp::IntegerVector& aux_y,
                            Rcpp::CharacterVector classes,
                            SEXP frame) {
  // construct out object
  Rcpp::List out(x.ncol() + aux_y.size());

  // first the joined columns (all x columns keep their location)
  DataFrameJoinVisitors join_visitors(x, y, by_x, by_y, true, false);
  for (int i = 0; i < by_x.size(); i++) {
    JoinVisitor* v = join_visitors.get(i);
    out[by_x[i] - 1] = v->subset(indices_x);
  }

  // then the auxiliary x columns (all x columns keep their location)
  DataFrameSubsetVisitors subset_x(DataFrameSelect(x, aux_x), frame);

  // convert indices_x to 1-based R indices
  int n_x = indices_x.size();
  Rcpp::IntegerVector indices_x_one_based(indices_x.size());
  for (int j = 0; j < n_x; j++) {
    indices_x_one_based[j] = indices_x[j] < 0 ? NA_INTEGER : (indices_x[j] + 1);
  }

  // materialize the first few columns
  for (int i = 0; i < aux_x.size(); i++) {
    out[aux_x[i] - 1] = subset_x.subset_one(i, indices_x_one_based);
  }

  // convert indices_y
  int n_y = indices_y.size();
  Rcpp::IntegerVector indices_y_one_based(indices_y.size());
  for (int j = 0; j < n_y; j++) {
    indices_y_one_based[j] = indices_y[j] < 0 ? NA_INTEGER : (indices_y[j] + 1);
  }

  // then the auxiliary y columns (all y columns keep their relative location)
  DataFrameSubsetVisitors subset_y(DataFrameSelect(y, aux_y), frame);
  for (int i = 0, k = x.ncol(); i < aux_y.size(); i++, k++) {
    out[k] = subset_y.subset_one(i, indices_y_one_based);
  }

  int nrows = indices_x.size();
  set_rownames(out, nrows);
  set_class(out, classes);

  return (SEXP)out;
}

template <typename TargetContainer, typename SourceContainer>
void push_back(TargetContainer& x, const SourceContainer& y) {
  x.insert(x.end(), y.begin(), y.end());
}
template <typename TargetContainer, typename SourceContainer>
void push_back_right(TargetContainer& x, const SourceContainer& y) {
  // x.insert( x.end(), y.begin(), y.end() );
  int n = y.size();
  for (int i = 0; i < n; i++) {
    x.push_back(-y[i] - 1);
  }
}

template <typename Container>
void push_back(Container& x, typename Container::value_type value, int n) {
  for (int i = 0; i < n; i++)
    x.push_back(value);
}

void check_by(const Rcpp::CharacterVector& by) {
  if (by.size() == 0) bad_arg("by", "must specify variables to join by");
}


void check_by(const Rcpp::IntegerVector& by) {
  if (by.size() == 0) bad_arg("by", "must specify variables to join by");
}

}

// [[Rcpp::export(rng = false)]]
Rcpp::List nest_join_impl(Rcpp::DataFrame x, Rcpp::DataFrame y,
                          Rcpp::IntegerVector by_x, Rcpp::IntegerVector by_y,
                          Rcpp::IntegerVector aux_y,
                          Rcpp::String yname,
                          SEXP frame
                         ) {

  dplyr::check_by(by_x);

  typedef dplyr::VisitorSetIndexMap<dplyr::DataFrameJoinVisitors, std::vector<int> > Map;
  dplyr::DataFrameJoinVisitors visitors(x, y, by_x, by_y, false, true);
  Map map(visitors);

  int n_x = x.nrows(), n_y = y.nrows();

  dplyr::train_push_back_right(map, n_y);

  Rcpp::List list_col(n_x);

  dplyr::DataFrameSubsetVisitors y_subset_visitors(dplyr::DataFrameSelect(y, aux_y), frame);

  // to deal with the case where multiple rows of x match rows in y
  dplyr_hash_map<int, SEXP> resolved_map(y_subset_visitors.size());

  // empty integer vector
  Rcpp::IntegerVector empty(0);

  for (int i = 0; i < n_x; i++) {

    // check if the i row of x matches rows in y
    Map::iterator it = map.find(i);
    if (it != map.end()) {

      // then check if we have already seen that match
      dplyr_hash_map<int, SEXP>::iterator rit = resolved_map.find(it->first);
      if (rit == resolved_map.end()) {
        // first time we see the match, perform the subset
        const std::vector<int>& indices_negative = it->second;
        int n = indices_negative.size();
        Rcpp::IntegerVector indices_one_based(n);
        for (int j = 0; j < n; j++) {
          indices_one_based[j] = -indices_negative[j];
        }

        resolved_map[it->first] = list_col[i] = y_subset_visitors.subset_all(indices_one_based);
      } else {
        // we have seen that match already, just lazy duplicate the tibble that is
        // stored in the resolved map
        list_col[i] = Rf_lazy_duplicate(rit->second);
      }

    } else {
      list_col[i] = y_subset_visitors.subset_all(empty);
    }
  }

  int ncol_x = x.size();
  Rcpp::List out(ncol_x + 1);
  Rcpp::Shield<SEXP> x_names(Rf_getAttrib(x, dplyr::symbols::names));
  Rcpp::Shield<SEXP> new_names(Rf_allocVector(STRSXP, ncol_x + 1));

  for (int i = 0; i < ncol_x; i++) {
    out[i] = x[i];
    SET_STRING_ELT(new_names, i, STRING_ELT(x_names, i));
  }
  out[ncol_x] = list_col ;
  SET_STRING_ELT(new_names, ncol_x, yname.get_sexp());
  Rf_namesgets(out, new_names);

  dplyr::copy_attrib(out, x, R_ClassSymbol);
  dplyr::copy_attrib(out, x, R_RowNamesSymbol);
  dplyr::GroupedDataFrame::copy_groups(out, x) ;

  return out;
}
