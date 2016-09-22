#include <dplyr.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/tbl_cpp.h>

#include <dplyr/DataFrameJoinVisitors.h>

#include <dplyr/Result/GroupedCallProxy.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/train.h>

using namespace Rcpp;
using namespace dplyr;

template <typename Index>
DataFrame subset_join(DataFrame x, DataFrame y,
                      const Index& indices_x, const Index& indices_y,
                      CharacterVector by_x, CharacterVector by_y ,
                      const std::string& suffix_x, const std::string& suffix_y,
                      CharacterVector classes) {
  // first the joined columns
  DataFrameJoinVisitors join_visitors(x, y, by_x, by_y, false);
  int n_join_visitors = join_visitors.size();

  // then columns from x but not y
  CharacterVector all_x_columns = x.names();
  std::vector<bool> joiner(all_x_columns.size());
  CharacterVector x_columns(all_x_columns.size() - n_join_visitors);
  IntegerVector xm = r_match(all_x_columns, by_x);
  for (int i=0, k=0; i<all_x_columns.size(); i++) {
    if (xm[i] == NA_INTEGER) {
      joiner[i] = false;
      x_columns[k++] = all_x_columns[i];
    } else {
      joiner[i] = true;
    }
  }
  DataFrameSubsetVisitors visitors_x(x, x_columns);
  int nv_x = visitors_x.size();

  // then columns from y but not x
  CharacterVector all_y_columns = y.names();
  CharacterVector y_columns(all_y_columns.size() - n_join_visitors);
  IntegerVector ym = r_match(all_y_columns, by_y);
  for (int i=0, k=0; i<all_y_columns.size(); i++) {
    if (ym[i] == NA_INTEGER) {
      y_columns[k++] = all_y_columns[i];
    }
  }
  DataFrameSubsetVisitors visitors_y(y, y_columns);

  int nv_y = visitors_y.size();

  // construct out object
  int nrows = indices_x.size();
  List out(n_join_visitors+nv_x+nv_y);
  CharacterVector names(n_join_visitors+nv_x+nv_y);

  int index_join_visitor = 0;
  int index_x_visitor = 0;
  // ---- join visitors
  for (int i=0; i<all_x_columns.size(); i++) {
    String col_name = all_x_columns[i];
    if (joiner[i]) {
      JoinVisitor* v = join_visitors.get(xm[i]-1);
      out[i] = v->subset(indices_x);
      index_join_visitor++;
    } else {

      while (
        (std::find(y_columns.begin(), y_columns.end(), col_name.get_sexp()) != y_columns.end()) ||
        (std::find(names.begin(), names.begin() + i, col_name.get_sexp()) != names.begin() + i)
      ) {
        col_name += suffix_x;
      }
      out[i] = visitors_x.get(index_x_visitor)->subset(indices_x);
      index_x_visitor++;
    }
    names[i] = col_name;
  }

  int k = index_join_visitor +  index_x_visitor;
  for (int i=0; i<nv_y; i++, k++) {
    String col_name = y_columns[i];

    // we suffix by .y if this column is in x_columns

    while (
      (std::find(all_x_columns.begin(), all_x_columns.end(), col_name.get_sexp()) != all_x_columns.end()) ||
      (std::find(names.begin(), names.begin() + k, col_name.get_sexp()) != names.begin() + k)
    ) {
      col_name += suffix_y;
    }

    out[k] = visitors_y.get(i)->subset(indices_y);
    names[k] = col_name;
  }
  out.attr("class") = classes;
  set_rownames(out, nrows);
  out.names() = names;

  SEXP vars = x.attr("vars");
  if (!Rf_isNull(vars))
    out.attr("vars") = vars;

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
  for (int i=0; i<n; i++) {
    x.push_back(-y[i]-1);
  }
}

template <typename Container>
void push_back(Container& x, typename Container::value_type value, int n) {
  for (int i=0; i<n; i++)
    x.push_back(value);
}

// [[Rcpp::export]]
DataFrame semi_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  int n_y = y.nrows();
  // this will collect indices from rows in x that match rows in y
  std::vector<int> indices;
  for (int i=0; i<n_y; i++) {
    // find a row in x that matches row i from y
    Map::iterator it = map.find(-i-1);

    if (it != map.end()) {
      // collect the indices and remove them from the
      // map so that they are only found once.
      push_back(indices, it->second);

      map.erase(it);

    }
  }

  return subset(x, indices, x.names(), x.attr("class"));
}

// [[Rcpp::export]]
DataFrame anti_join_impl(DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, false);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  int n_y = y.nrows();
  // remove the rows in x that match
  for (int i=0; i<n_y; i++) {
    Map::iterator it = map.find(-i-1);
    if (it != map.end())
      map.erase(it);
  }

  // collect what's left
  std::vector<int> indices;
  for (Map::iterator it = map.begin(); it != map.end(); ++it)
    push_back(indices, it->second);

  return subset(x, indices, x.names(), x.attr("class"));
}

// [[Rcpp::export]]
DataFrame inner_join_impl(DataFrame x, DataFrame y,
                          CharacterVector by_x, CharacterVector by_y,
                          std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, true);
  Map map(visitors);

  int n_x = x.nrows(), n_y = y.nrows();

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  train_push_back_right(map, n_y);

  for (int i=0; i<n_x; i++) {
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
                     x.attr("class")
                    );
}

// [[Rcpp::export]]
DataFrame left_join_impl(DataFrame x, DataFrame y,
                         CharacterVector by_x, CharacterVector by_y,
                         std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, true);

  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_x = x.nrows();
  for (int i=0; i<n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1);
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
                     x.attr("class")
                    );
}

// [[Rcpp::export]]
DataFrame right_join_impl(DataFrame x, DataFrame y,
                          CharacterVector by_x, CharacterVector by_y,
                          std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(x, y, by_x, by_y, true);
  Map map(visitors);

  // train the map in terms of x
  train_push_back(map, x.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_y = y.nrows();
  for (int i=0; i<n_y; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1);
    if (it != map.end()) {
      push_back(indices_x,  it->second);
      push_back(indices_y, i, it->second.size());
    } else {
      indices_x.push_back(-i-1); // point to the i-th row in the right table
      indices_y.push_back(i);
    }
  }
  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     suffix_x, suffix_y,
                     x.attr("class")
                    );
}

// [[Rcpp::export]]
DataFrame full_join_impl(DataFrame x, DataFrame y,
                         CharacterVector by_x, CharacterVector by_y,
                         std::string& suffix_x, std::string& suffix_y) {
  if (by_x.size() == 0) stop("no variable to join by");
  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  DataFrameJoinVisitors visitors(y, x, by_y, by_x, true);
  Map map(visitors);

  // train the map in terms of y
  train_push_back(map, y.nrows());

  std::vector<int> indices_x;
  std::vector<int> indices_y;

  int n_x = x.nrows(), n_y = y.nrows();

  // get both the matches and the rows from left but not right
  for (int i=0; i<n_x; i++) {
    // find a row in y that matches row i in x
    Map::iterator it = map.find(-i-1);
    if (it != map.end()) {
      push_back(indices_y,  it->second);
      push_back(indices_x, i, it->second.size());
    } else {
      indices_y.push_back(-1); // mark NA
      indices_x.push_back(i);
    }
  }

  // train a new map in terms of x this time
  DataFrameJoinVisitors visitors2(x,y,by_x,by_y, false);
  Map map2(visitors2);
  train_push_back(map2, x.nrows());

  for (int i=0; i<n_y; i++) {
    // try to find row in x that matches this row of y
    Map::iterator it = map2.find(-i-1);
    if (it == map2.end()) {
      indices_x.push_back(-i-1);
      indices_y.push_back(i);
    }
  }

  return subset_join(x, y,
                     indices_x, indices_y,
                     by_x, by_y,
                     suffix_x, suffix_y,
                     x.attr("class")
                    );
}

typedef dplyr_hash_set<SEXP> SymbolSet;

inline SEXP check_filter_integer_result(SEXP tmp) {
  if (TYPEOF(tmp) != INTSXP &&  TYPEOF(tmp) != REALSXP && TYPEOF(tmp) != LGLSXP) {
    stop("slice condition does not evaluate to an integer or numeric vector. ");
  }
  return tmp;
}

class CountIndices {
public:
  CountIndices(int nr_, IntegerVector test_) : nr(nr_), test(test_), n_pos(0), n_neg(0) {

    for (int j=0; j<test.size(); j++) {
      int i = test[j];
      if (i > 0 && i <= nr) {
        n_pos++;
      } else if (i < 0 && i >= -nr) {
        n_neg++;
      }
    }

    if (n_neg > 0 && n_pos > 0) {
      stop("found %d positive indices and %d negative indices", n_pos, n_neg);
    }

  }

  inline bool is_positive() const {
    return n_pos > 0;
  }
  inline int get_n_positive() const {
    return n_pos;
  }
  inline int get_n_negative() const {
    return n_neg;
  }

private:
  int nr;
  IntegerVector test;
  int n_pos;
  int n_neg;
};

SEXP slice_grouped(GroupedDataFrame gdf, const LazyDots& dots) {
  typedef GroupedCallProxy<GroupedDataFrame, LazyGroupedSubsets> Proxy;

  const DataFrame& data = gdf.data();
  const Lazy& lazy = dots[0];
  Environment env = lazy.env();
  CharacterVector names = data.names();
  SymbolSet set;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i]));
  }

  // we already checked that we have only one expression
  Call call(lazy.expr());

  std::vector<int> indx;
  indx.reserve(1000);

  IntegerVector g_test;
  Proxy call_proxy(call, gdf, env);

  int ngroups = gdf.ngroups();
  GroupedDataFrame::group_iterator git = gdf.group_begin();
  for (int i=0; i<ngroups; i++, ++git) {
    SlicingIndex indices = *git;
    int nr = indices.size();
    g_test = check_filter_integer_result(call_proxy.get(indices));
    CountIndices counter(indices.size(), g_test);

    if (counter.is_positive()) {
      // positive indexing
      int ntest = g_test.size();
      for (int j=0; j<ntest; j++) {
        if (!(g_test[j] > nr || g_test[j] == NA_INTEGER)) {
          indx.push_back(indices[g_test[j]-1]);
        }
      }
    } else if (counter.get_n_negative() != 0) {
      // negative indexing
      std::set<int> drop;
      int n = g_test.size();
      for (int j=0; j<n; j++) {
        if (g_test[j] != NA_INTEGER)
          drop.insert(-g_test[j]);
      }
      int n_drop = drop.size();
      std::set<int>::const_iterator drop_it = drop.begin();

      int k = 0, j = 0;
      while (drop_it != drop.end()) {
        int next_drop = *drop_it - 1;
        while (j < next_drop) {
          indx.push_back(indices[j++]);
          k++;
        }
        j++;
        ++drop_it;
      }
      while (k < nr - n_drop) {
        indx.push_back(indices[j++]);
        k++;
      }

    }
  }
  DataFrame res = subset(data, indx, names, classes_grouped<GroupedDataFrame>());
  res.attr("vars")   = data.attr("vars");
  strip_index(res);

  return GroupedDataFrame(res).data();

}

SEXP slice_not_grouped(const DataFrame& df, const LazyDots& dots) {
  CharacterVector names = df.names();
  SymbolSet set;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i]));
  }
  const Lazy& lazy = dots[0];
  Call call(lazy.expr());
  CallProxy proxy(call, df, lazy.env());
  int nr = df.nrows();

  IntegerVector test = check_filter_integer_result(proxy.eval());

  int n = test.size();

  // count the positive and negatives
  CountIndices counter(nr, test);

  // just positives -> one based subset
  if (counter.is_positive()) {
    int n_pos = counter.get_n_positive();
    std::vector<int> idx(n_pos);
    int j=0;
    for (int i=0; i<n_pos; i++) {
      while (test[j] > nr || test[j] == NA_INTEGER) j++;
      idx[i] = test[j++] - 1;
    }

    return subset(df, idx, df.names(), classes_not_grouped());
  }

  // special case where only NA
  if (counter.get_n_negative() == 0) {
    std::vector<int> indices;
    DataFrame res = subset(df, indices, df.names(), classes_not_grouped());
    return res;
  }

  // just negatives (out of range is dealt with early in CountIndices).
  std::set<int> drop;
  for (int i=0; i<n; i++) {
    if (test[i] != NA_INTEGER)
      drop.insert(-test[i]);
  }
  int n_drop = drop.size();
  std::vector<int> indices(nr - n_drop);
  std::set<int>::const_iterator drop_it = drop.begin();

  int i = 0, j = 0;
  while (drop_it != drop.end()) {
    int next_drop = *drop_it - 1;
    while (j < next_drop) {
      indices[i++] = j++;
    }
    j++;
    ++drop_it;
  }
  while (i < nr - n_drop) {
    indices[i++] = j++;
  }

  DataFrame res = subset(df, indices, df.names(), classes_not_grouped());
  return res;

}

// [[Rcpp::export]]
SEXP slice_impl(DataFrame df, LazyDots dots) {
  if (dots.size() == 0) return df;
  if (dots.size() != 1)
    stop("slice only accepts one expression");
  if (is<GroupedDataFrame>(df)) {
    return slice_grouped(GroupedDataFrame(df), dots);
  } else {
    return slice_not_grouped(df, dots);
  }
}
