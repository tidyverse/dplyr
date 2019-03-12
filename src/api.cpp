#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/match.h>

#include <dplyr/visitors/CharacterVectorOrderer.h>

#include <dplyr/visitors/vector/visitor_impl.h>

#include <dplyr/visitors/join/JoinVisitor.h>
#include <dplyr/visitors/join/JoinVisitorImpl.h>
#include <dplyr/visitors/join/DataFrameJoinVisitors.h>

#include <tools/bad.h>

namespace dplyr {

DataFrameVisitors::DataFrameVisitors(const Rcpp::DataFrame& data_) :
  data(data_),
  visitors(),
  visitor_names(vec_names_or_empty(data))
{

  for (int i = 0; i < data.size(); i++) {
    VectorVisitor* v = visitor(data[i]);
    visitors.push_back(v);
  }
}

DataFrameVisitors::DataFrameVisitors(const DataFrame& data_, const SymbolVector& names) :
  data(data_),
  visitors(),
  visitor_names(names)
{

  int n = names.size();
  CharacterVector data_names = vec_names_or_empty(data);
  IntegerVector indices = names.match_in_table(data_names);

  for (int i = 0; i < n; i++) {
    if (indices[i] == NA_INTEGER) {
      bad_col(names[i], "is unknown");
    }
    SEXP column = data[indices[i] - 1];
    visitors.push_back(visitor(column));
  }

}

DataFrameVisitors::DataFrameVisitors(const DataFrame& data_, const IntegerVector& indices) :
  data(data_),
  visitors(),
  visitor_names()
{

  CharacterVector data_names = vec_names_or_empty(data);

  int n = indices.size();
  for (int i = 0; i < n; i++) {
    int pos = check_range_one_based(indices[i], data.size());

    VectorVisitor* v = visitor(data[pos - 1]);
    visitors.push_back(v);
    visitor_names.push_back(data_names[pos - 1]);
  }
}

DataFrameVisitors::DataFrameVisitors(const DataFrame& data_,  int n) :
  data(data_),
  visitors(n),
  visitor_names(n)
{

  CharacterVector data_names = vec_names_or_empty(data);

  for (int i = 0; i < n; i++) {
    visitors[i] = visitor(data[i]);
    visitor_names.set(i, data_names[i]);
  }
}

DataFrameJoinVisitors::DataFrameJoinVisitors(const DataFrame& left_, const DataFrame& right_, const SymbolVector& names_left, const SymbolVector& names_right, bool warn_, bool na_match) :
  left(left_), right(right_),
  visitor_names_left(names_left),
  visitor_names_right(names_right),
  visitors(names_left.size()),
  warn(warn_)
{
  Shield<SEXP> left_names(RCPP_GET_NAMES(left));
  Shield<SEXP> right_names(RCPP_GET_NAMES(right));
  IntegerVector indices_left  = names_left.match_in_table((SEXP)left_names);
  IntegerVector indices_right = names_right.match_in_table((SEXP)right_names);

  const int nvisitors = indices_left.size();
  if (indices_right.size() != nvisitors) {
    stop("Different size of join column index vectors");
  }

  for (int i = 0; i < nvisitors; i++) {
    const SymbolString& name_left  = names_left[i];
    const SymbolString& name_right = names_right[i];

    if (indices_left[i] == NA_INTEGER) {
      stop("'%s' column not found in lhs, cannot join", name_left.get_utf8_cstring());
    }
    if (indices_right[i] == NA_INTEGER) {
      stop("'%s' column not found in rhs, cannot join", name_right.get_utf8_cstring());
    }

    visitors[i] =
      join_visitor(
        Column(left[indices_left[i] - 1], name_left),
        Column(right[indices_right[i] - 1], name_right),
        warn, na_match
      );
  }
}

DataFrameJoinVisitors::DataFrameJoinVisitors(
  const DataFrame& left_, const DataFrame& right_,
  const IntegerVector& indices_left, const IntegerVector& indices_right,
  bool warn_, bool na_match
) :
  left(left_), right(right_),
  visitor_names_left(),
  visitor_names_right(),
  visitors(indices_left.size()),
  warn(warn_)
{
  if (indices_right.size() != size()) {
    stop("Different size of join column index vectors");
  }

  SymbolVector left_names = left.names();
  SymbolVector right_names = right.names();

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
  stop("visitor not found for name '%s' ", name.get_utf8_cstring());
}

int DataFrameJoinVisitors::size() const {
  return visitors.size();
}

CharacterVectorOrderer::CharacterVectorOrderer(const CharacterVector& data) :
  orders(no_init(data.size()))
{
  int n = data.size();
  if (n == 0) return;

  dplyr_hash_set<SEXP> set(n);

  // 1 - gather unique SEXP pointers from data
  SEXP* p_data = Rcpp::internal::r_vector_start<STRSXP>(data);
  SEXP previous = *p_data++;
  set.insert(previous);
  for (int i = 1; i < n; i++, p_data++) {
    SEXP s = *p_data;

    // we've just seen this string, keep going
    if (s == previous) continue;

    // is this string in the set already
    set.insert(s);
    previous = s;
  }

  // retrieve unique strings from the set
  int n_uniques = set.size();
  LOG_VERBOSE << "Sorting " <<  n_uniques << " unique character elements";

  CharacterVector uniques(set.begin(), set.end());

  static Function sort("sort", R_BaseEnv);
  Language call(sort, uniques);
  CharacterVector s_uniques = call.fast_eval();

  // order the uniques with a callback to R
  IntegerVector o = r_match(uniques, s_uniques);

  // combine uniques and o into a hash map for fast retrieval
  dplyr_hash_map<SEXP, int> map(n_uniques);
  for (int i = 0; i < n_uniques; i++) {
    map.insert(std::make_pair(uniques[i], o[i]));
  }

  // grab min ranks
  p_data = Rcpp::internal::r_vector_start<STRSXP>(data);
  previous = *p_data++;

  int o_pos;
  orders[0] = o_pos = map.find(previous)->second;

  for (int i = 1; i < n; ++i, ++p_data) {
    SEXP s = *p_data;
    if (s == previous) {
      orders[i] = o_pos;
      continue;
    }
    previous = s;
    orders[i] = o_pos = map.find(s)->second;
  }

}

CharacterVector get_uniques(const CharacterVector& left, const CharacterVector& right) {
  int nleft = left.size(), nright = right.size();
  int n = nleft + nright;

  CharacterVector big(no_init(n));
  CharacterVector::iterator it = big.begin();
  std::copy(left.begin(), left.end(), it);
  std::copy(right.begin(), right.end(), it + nleft);

  static Function unique("unique", R_BaseEnv);
  return Language(unique, big).fast_eval();
}

}
