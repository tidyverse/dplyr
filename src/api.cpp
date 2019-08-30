#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/match.h>

#include <dplyr/visitors/join/JoinVisitor.h>
#include <dplyr/visitors/join/JoinVisitorImpl.h>
#include <dplyr/visitors/join/DataFrameJoinVisitors.h>

#include <tools/bad.h>

namespace dplyr {

DataFrameJoinVisitors::DataFrameJoinVisitors(const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_, const SymbolVector& names_left, const SymbolVector& names_right, bool warn_, bool na_match) :
  left(left_), right(right_),
  visitor_names_left(names_left),
  visitor_names_right(names_right),
  visitors(names_left.size()),
  warn(warn_)
{
  Rcpp::Shield<SEXP> left_names(RCPP_GET_NAMES(left));
  Rcpp::Shield<SEXP> right_names(RCPP_GET_NAMES(right));

  Rcpp::Shield<SEXP> indices_left(names_left.match_in_table((SEXP)left_names));
  Rcpp::Shield<SEXP> indices_right(names_right.match_in_table((SEXP)right_names));
  int* p_indices_left = INTEGER(indices_left);
  int* p_indices_right = INTEGER(indices_right);

  R_xlen_t nvisitors = XLENGTH(indices_left);
  if (XLENGTH(indices_right) != nvisitors) {
    Rcpp::stop("Different size of join column index vectors");
  }

  for (int i = 0; i < nvisitors; i++) {
    const SymbolString& name_left  = names_left[i];
    const SymbolString& name_right = names_right[i];

    if (p_indices_left[i] == NA_INTEGER) {
      Rcpp::stop("'%s' column not found in lhs, cannot join", name_left.get_utf8_cstring());
    }
    if (p_indices_right[i] == NA_INTEGER) {
      Rcpp::stop("'%s' column not found in rhs, cannot join", name_right.get_utf8_cstring());
    }

    visitors[i] = join_visitor(
                    Column(left[p_indices_left[i] - 1], name_left),
                    Column(right[p_indices_right[i] - 1], name_right),
                    warn, na_match
                  );
  }
}

DataFrameJoinVisitors::DataFrameJoinVisitors(
  const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_,
  const Rcpp::IntegerVector& indices_left, const Rcpp::IntegerVector& indices_right,
  bool warn_, bool na_match
) :
  left(left_), right(right_),
  visitor_names_left(),
  visitor_names_right(),
  visitors(indices_left.size()),
  warn(warn_)
{
  if (indices_right.size() != size()) {
    Rcpp::stop("Different size of join column index vectors");
  }

  SymbolVector left_names(Rf_getAttrib(left, symbols::names));
  SymbolVector right_names(Rf_getAttrib(right, symbols::names));

  for (int i = 0; i < size(); i++) {
    const int index_left = check_range_one_based(indices_left[i], left.size());
    const int index_right = check_range_one_based(indices_right[i], right.size());

    const SymbolString& name_left = left_names[index_left - 1];
    const SymbolString& name_right = right_names[index_right - 1];

    visitors[i] =
      join_visitor(
        Column(left[index_left - 1], name_left),
        Column(right[index_right - 1], name_right),
        warn, na_match
      );

    visitor_names_left.push_back(name_left);
    visitor_names_right.push_back(name_right);
  }
}

JoinVisitor* DataFrameJoinVisitors::get(int k) const {
  return visitors[k];
}

JoinVisitor* DataFrameJoinVisitors::get(const SymbolString& name) const {
  for (int i = 0; i < size(); i++) {
    if (name == visitor_names_left[i]) return get(i);
  }
  Rcpp::stop("visitor not found for name '%s' ", name.get_utf8_cstring());
}

int DataFrameJoinVisitors::size() const {
  return visitors.size();
}

}
