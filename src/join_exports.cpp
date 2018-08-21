#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/match.h>

#include <tools/Quosure.h>

#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/visitors/join/DataFrameJoinVisitors.h>

#include <tools/train.h>
#include <tools/bad.h>

using namespace Rcpp;
using namespace dplyr;
#include <tools/debug.h>

template <typename Index>
DataFrame subset_join(DataFrame x, DataFrame y,
                      const Index& indices_x, const Index& indices_y,
                      const IntegerVector& by_x, const IntegerVector& by_y,
                      const IntegerVector& aux_x, const IntegerVector& aux_y,
                      CharacterVector classes) {
  // construct out object
  List out(x.ncol() + aux_y.size());

  // first the joined columns (all x columns keep their location)
  DataFrameJoinVisitors join_visitors(x, y, by_x, by_y, true, false);
  for (int i = 0; i < by_x.size(); i++) {
    JoinVisitor* v = join_visitors.get(i);
    out[by_x[i] - 1] = v->subset(indices_x);
  }

  // then the auxiliary x columns (all x columns keep their location)
  DataFrameSubsetVisitors visitors_x(x, aux_x);
  for (int i = 0; i < aux_x.size(); i++) {
    SubsetVectorVisitor* const v = visitors_x.get(i);
    out[aux_x[i] - 1] = v->subset(indices_x);
  }

  // then the auxiliary y columns (all y columns keep their relative location)
  DataFrameSubsetVisitors visitors_y(y, aux_y);
  for (int i = 0, k = x.ncol(); i < visitors_y.size(); i++, k++) {
    SubsetVectorVisitor* const v = visitors_y.get(i);
    out[k] = v->subset(indices_y);
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

void check_by(const CharacterVector& by) {
  if (by.size() == 0) bad_arg("by", "must specify variables to join by");
}

// [[Rcpp::export]]
DataFrame semi_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y, bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, SymbolVector(by_x), SymbolVector(by_y), true, na_match);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  int n_y = y.nrows();
  // this will collect indices from rows in x that match rows in y
  std::vector<int> indices;
  indices.reserve(x.nrows());
  for (int i = 0; i < n_y; i++) {
    // find a row in x that matches row i from y
    Map::iterator it = map.find(-i - 1);

    if (it != map.end()) {
      // collect the indices and remove them from the
      // map so that they are only found once.
      push_back(indices, it->second);

      map.erase(it);

    }
  }

  std::sort(indices.begin(), indices.end());

  const DataFrame& out = subset(x, indices, get_class(x));
  // strip_index(out);
  return out;
}

// [[Rcpp::export]]
DataFrame anti_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y, bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, SymbolVector(by_x), SymbolVector(by_y), true, na_match);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  int n_y = y.nrows();
  // remove the rows in x that match
  for (int i = 0; i < n_y; i++) {
    Map::iterator it = map.find(-i - 1);
    if (it != map.end())
      map.erase(it);
  }

  // collect what's left
  std::vector<int> indices;
  indices.reserve(map.size());
  for (Map::iterator it = map.begin(); it != map.end(); ++it)
    push_back(indices, it->second);

  std::sort(indices.begin(), indices.end());

  const DataFrame& out = subset(x, indices, get_class(x));

  return out;
}

void check_by(const IntegerVector& by) {
  if (by.size() == 0) bad_arg("by", "must specify variables to join by");
}

// [[Rcpp::export]]
DataFrame inner_join_impl(DataFrame x, DataFrame y,
                          IntegerVector by_x, IntegerVector by_y,
                          IntegerVector aux_x, IntegerVector aux_y,
                          bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false, na_match);
  Map map(visitors);

  int n_x = x.nrows(), n_y = y.nrows();

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  train_push_back_right(map, n_y);

  for (int i = 0; i < n_x; i++) {
    Map::iterator it = map.find(i);
    if (it != map.end()) {
      push_back_right(indices_y, it->second);
      push_back(indices_x, i, it->second.size());
    }
  }

  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     aux_x, aux_y,
                     get_class(x)
                    );
}

inline int reverse_index(int i) {
  return -i - 1;
}

// [[Rcpp::export]]
List nest_join_impl(DataFrame x, DataFrame y,
                    IntegerVector by_x, IntegerVector by_y,
                    IntegerVector aux_y,
                    String yname
                   ) {

  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false, true);
  Map map(visitors);

  int n_x = x.nrows(), n_y = y.nrows();

  train_push_back_right(map, n_y);

  List list_col(n_x) ;

  DataFrameSubsetVisitors y_subset_visitors(y, aux_y);

  // to deal with the case where multiple rows of x match rows in y
  dplyr_hash_map<int, SEXP> resolved_map(y_subset_visitors.size());

  for (int i = 0; i < n_x; i++) {

    // check if the i row of x matches rows in y
    Map::iterator it = map.find(i);
    if (it != map.end()) {

      // then check if we have already seen that match
      dplyr_hash_map<int, SEXP>::iterator rit = resolved_map.find(it->first);
      if (rit == resolved_map.end()) {
        // first time we see the match, so transform the indices that were collected
        // so that they are on the 0-based positive space, then subset y
        std::transform(it->second.begin(), it->second.end(), it->second.begin(), reverse_index);
        resolved_map[it->first] = list_col[i] = y_subset_visitors.subset(it->second, Rf_getAttrib(y, R_ClassSymbol));
      } else {
        // we have seen that match already, just lazy duplicate the tibble that is
        // stored in the resolved map
        list_col[i] = Rf_lazy_duplicate(rit->second);
      }

    } else {
      list_col[i] = y_subset_visitors.subset(std::vector<int>(0), Rf_getAttrib(y, R_ClassSymbol));
    }
  }

  int ncol_x = x.size();
  List out(ncol_x + 1);
  CharacterVector names_x = x.names();
  for (int i = 0; i < ncol_x; i++) {
    out[i] = x[i];
  }
  names_x.push_back(yname) ;
  out[ncol_x] = list_col ;
  out.names() = names_x;
  out.attr("class") = x.attr("class");
  out.attr("row.names") = x.attr("row.names");

  GroupedDataFrame::copy_groups(out, x) ;

  return out;
}



// [[Rcpp::export]]
DataFrame left_join_impl(DataFrame x, DataFrame y,
                         IntegerVector by_x, IntegerVector by_y,
                         IntegerVector aux_x, IntegerVector aux_y,
                         bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, false, na_match);

  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_x = x.nrows();
  for (int i = 0; i < n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i - 1);
    if (it != map.end()) {
      push_back(indices_y,  it->second);
      push_back(indices_x, i, it->second.size());
    } else {
      indices_y.push_back(-1); // mark NA
      indices_x.push_back(i);
    }
  }

  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     aux_x, aux_y,
                     get_class(x)
                    );
}

// [[Rcpp::export]]
DataFrame right_join_impl(DataFrame x, DataFrame y,
                          IntegerVector by_x, IntegerVector by_y,
                          IntegerVector aux_x, IntegerVector aux_y,
                          bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false, na_match);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_y = y.nrows();
  for (int i = 0; i < n_y; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i - 1);
    if (it != map.end()) {
      push_back(indices_x,  it->second);
      push_back(indices_y, i, it->second.size());
    } else {
      indices_x.push_back(-i - 1); // point to the i-th row in the right table
      indices_y.push_back(i);
    }
  }
  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     aux_x, aux_y,
                     get_class(x)
                    );
}

// [[Rcpp::export]]
DataFrame full_join_impl(DataFrame x, DataFrame y,
                         IntegerVector by_x, IntegerVector by_y,
                         IntegerVector aux_x, IntegerVector aux_y,
                         bool na_match) {
  check_by(by_x);

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, false, na_match);
  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_x = x.nrows(), n_y = y.nrows();

  // get both the matches and the rows from left but not right
  for (int i = 0; i < n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i - 1);
    if (it != map.end()) {
      push_back(indices_y,  it->second);
      push_back(indices_x, i, it->second.size());
    } else {
      indices_y.push_back(-1); // mark NA
      indices_x.push_back(i);
    }
  }

  // train a new map in terms of x this time
  DataFrameJoinVisitors visitors2(x, y, by_x, by_y, false, na_match);
  Map map2(visitors2);
  train_push_back(map2, x.nrows());

  for (int i = 0; i < n_y; i++) {
    // try to find row in x that matches this row of y
    Map::iterator it = map2.find(-i - 1);
    if (it == map2.end()) {
      indices_x.push_back(-i - 1);
      indices_y.push_back(i);
    }
  }

  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     aux_x, aux_y,
                     get_class(x)
                    );
}
