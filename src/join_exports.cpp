#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/match.h>

#include <tools/Quosure.h>

#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/DataFrameJoinVisitors.h>

#include <dplyr/train.h>

#include <dplyr/bad.h>

using namespace Rcpp;
using namespace dplyr;

template <typename Index>
DataFrame subset_join(DataFrame x, DataFrame y,
                      const Index& indices_x, const Index& indices_y,
                      const IntegerVector& by_x, const IntegerVector& by_y,
                      const std::string& suffix_x, const std::string& suffix_y,
                      CharacterVector classes) {
  if (suffix_x.length() == 0 && suffix_y.length() == 0) {
    bad_arg("suffix", "can't be empty string for both `x` and `y` suffixes");
  }

  // first the joined columns
  DataFrameJoinVisitors join_visitors(x, y, by_x, by_y, true, false);
  int n_join_visitors = join_visitors.size();

  // then columns from x but not y
  CharacterVector all_x_columns = x.names();
  std::vector<bool> joiner(all_x_columns.size());
  CharacterVector x_columns(all_x_columns.size() - n_join_visitors);
  IntegerVector xm(all_x_columns.size(), NA_INTEGER);
  for (int by = 0; by < by_x.size(); ++by) {
    const int pos = by_x[by];
    check_range_one_based(pos, xm.size());
    xm[pos - 1] = by + 1;
  }
  for (int i = 0, k = 0; i < all_x_columns.size(); i++) {
    if (xm[i] == NA_INTEGER) {
      joiner[i] = false;
      x_columns[k++] = all_x_columns[i];
    } else {
      joiner[i] = true;
    }
  }

  // then columns from y but not x
  CharacterVector all_y_columns = y.names();
  CharacterVector y_columns(all_y_columns.size() - n_join_visitors);
  IntegerVector ym(all_y_columns.size(), NA_INTEGER);
  for (int by = 0; by < by_y.size(); ++by) {
    const int pos = by_y[by];
    check_range_one_based(pos, ym.size());
    ym[pos - 1] = by + 1;
  }
  for (int i = 0, k = 0; i < all_y_columns.size(); i++) {
    if (ym[i] == NA_INTEGER) {
      y_columns[k++] = all_y_columns[i];
    }
  }

  // construct out object
  int nrows = indices_x.size();
  List out(all_x_columns.size() + y_columns.size());
  CharacterVector names(out.size());

  int index_join_visitor = 0;
  int index_x_visitor = 0;
  DataFrameSubsetVisitors visitors_x(x, SymbolVector(x_columns));

  // ---- join visitors
  for (int i = 0; i < all_x_columns.size(); i++) {
    String col_name = all_x_columns[i];
    if (joiner[i]) {
      JoinVisitor* v = join_visitors.get(xm[i] - 1);
      out[i] = v->subset(indices_x);
      index_join_visitor++;
    } else {

      // we suffix by .x if this column is in y_columns (and if the suffix is not empty)
      if (suffix_x.length() > 0) {
        while (
          (std::find(y_columns.begin(), y_columns.end(), col_name.get_sexp()) != y_columns.end()) ||
          (std::find(names.begin(), names.begin() + i, col_name.get_sexp()) != names.begin() + i)
        ) {
          col_name += suffix_x;
        }
      }

      out[i] = visitors_x.get(index_x_visitor)->subset(indices_x);
      index_x_visitor++;
    }
    names[i] = col_name;
  }

  DataFrameSubsetVisitors visitors_y(y, SymbolVector(y_columns));
  for (int i = 0, k = all_x_columns.size(); i < y_columns.size(); i++, k++) {
    String col_name = y_columns[i];

    // we suffix by .y if this column is in x_columns (and if the suffix is not empty)
    if (suffix_y.length() > 0) {
      while (
        (std::find(all_x_columns.begin(), all_x_columns.end(), col_name.get_sexp()) != all_x_columns.end()) ||
        (std::find(names.begin(), names.begin() + k, col_name.get_sexp()) != names.begin() + k)
      ) {
        col_name += suffix_y;
      }
    }

    out[k] = visitors_y.get(i)->subset(indices_y);
    names[k] = col_name;
  }
  set_class(out, classes);
  set_rownames(out, nrows);
  out.names() = names;

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
  strip_index(out);
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
  strip_index(out);
  return out;
}

void check_by(const IntegerVector& by) {
  if (by.size() == 0) bad_arg("by", "must specify variables to join by");
}

// [[Rcpp::export]]
DataFrame inner_join_impl(DataFrame x, DataFrame y,
                          IntegerVector by_x, IntegerVector by_y,
                          std::string& suffix_x, std::string& suffix_y,
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
                     suffix_x, suffix_y,
                     get_class(x)
                    );
}

// [[Rcpp::export]]
DataFrame left_join_impl(DataFrame x, DataFrame y,
                         IntegerVector by_x, IntegerVector by_y,
                         std::string& suffix_x, std::string& suffix_y,
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
                     suffix_x, suffix_y,
                     get_class(x)
                    );
}

// [[Rcpp::export]]
DataFrame right_join_impl(DataFrame x, DataFrame y,
                          IntegerVector by_x, IntegerVector by_y,
                          std::string& suffix_x, std::string& suffix_y,
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
                     suffix_x, suffix_y,
                     get_class(x)
                    );
}

// [[Rcpp::export]]
DataFrame full_join_impl(DataFrame x, DataFrame y,
                         IntegerVector by_x, IntegerVector by_y,
                         std::string& suffix_x, std::string& suffix_y,
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
                     suffix_x, suffix_y,
                     get_class(x)
                    );
}
