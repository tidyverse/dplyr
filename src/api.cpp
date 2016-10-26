#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/hash.h>
#include <tools/match.h>

#include <dplyr/CharacterVectorOrderer.h>

#include <dplyr/tbl_cpp.h>
#include <dplyr/visitor_impl.h>

#include <dplyr/JoinVisitorImpl.h>

#include <dplyr/Hybrid.h>

#include <dplyr/Result/Result.h>
#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/DataFrameJoinVisitors.h>

namespace dplyr {

  DataFrameVisitors::DataFrameVisitors(const Rcpp::DataFrame& data_) :
    data(data_),
    visitors(),
    visitor_names(data.names()),
    nvisitors(visitor_names.size())
  {

    for (int i=0; i<nvisitors; i++) {
      VectorVisitor* v = visitor(data[i]);
      visitors.push_back(v);
    }
  }

  DataFrameVisitors::DataFrameVisitors(const Rcpp::DataFrame& data_, const Rcpp::CharacterVector& names) :
    data(data_),
    visitors(),
    visitor_names(names),
    nvisitors(visitor_names.size())
  {

    std::string name;
    int n = names.size();
    IntegerVector indices  = r_match(names,  RCPP_GET_NAMES(data));

    for (int i=0; i<n; i++) {
      if (indices[i] == NA_INTEGER) {
        name = (String)names[i];
        stop("unknown column '%s' ", name);
      }
      SEXP column = data[indices[i]-1];
      visitors.push_back(visitor(column));
    }

  }

  void DataFrameVisitors::structure(List& x, int nrows, CharacterVector classes) const {
    x.attr("class") = classes;
    set_rownames(x, nrows);
    x.names() = visitor_names;
    SEXP vars = data.attr("vars");
    if (!Rf_isNull(vars))
      x.attr("vars") = vars;
  }

  DataFrameJoinVisitors::DataFrameJoinVisitors(const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_, Rcpp::CharacterVector names_left, Rcpp::CharacterVector names_right, bool warn_) :
    left(left_), right(right_),
    visitor_names_left(names_left),
    visitor_names_right(names_right),
    nvisitors(names_left.size()),
    visitors(nvisitors),
    warn(warn_)
  {
    std::string name_left, name_right;

    IntegerVector indices_left  = r_match(names_left,  RCPP_GET_NAMES(left));
    IntegerVector indices_right = r_match(names_right, RCPP_GET_NAMES(right));

    for (int i=0; i<nvisitors; i++) {
      name_left  = names_left[i];
      name_right = names_right[i];

      if (indices_left[i] == NA_INTEGER) {
        stop("'%s' column not found in lhs, cannot join", name_left);
      }
      if (indices_right[i] == NA_INTEGER) {
        stop("'%s' column not found in rhs, cannot join", name_right);
      }

      visitors[i] = join_visitor(left[indices_left[i]-1], right[indices_right[i]-1], name_left, name_right, warn);
    }
  }

  Symbol extract_column(SEXP arg, const Environment& env) {
    RObject value;
    if (TYPEOF(arg) == LANGSXP && CAR(arg) == Rf_install("~")) {
      if (Rf_length(arg) != 2 || TYPEOF(CADR(arg)) != SYMSXP)
        stop("unhandled formula in column");
      value = CharacterVector::create(PRINTNAME(CADR(arg)));
    } else {
      value = Rcpp_eval(arg, env);
    }
    if (is<Symbol>(value)) {
      value = CharacterVector::create(PRINTNAME(value));
    }
    if (!is<String>(value)) {
      stop("column must return a single string");
    }
    Symbol res(STRING_ELT(value,0));
    return res;
  }

  Symbol get_column(SEXP arg, const Environment& env, const ILazySubsets& subsets) {
    Symbol res = extract_column(arg, env);
    if (!subsets.count(res)) {
      stop("result of column() expands to a symbol that is not a variable from the data: %s", CHAR(PRINTNAME(res)));
    }
    return res;
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
    for (int i=1; i<n; i++, p_data++) {
      SEXP s = *p_data;

      // we've just seen this string, keep going
      if (s == previous) continue;

      // is this string in the set already
      set.insert(s);
      previous = s;
    }

    // retrieve unique strings from the set
    int n_uniques = set.size();
    CharacterVector uniques(set.begin(), set.end());
    CharacterVector s_uniques = Language("sort", uniques).fast_eval();

    // order the uniques with a callback to R
    IntegerVector o = r_match(uniques, s_uniques);

    // combine uniques and o into a hash map for fast retrieval
    dplyr_hash_map<SEXP,int> map(n_uniques);
    for (int i=0; i<n_uniques; i++) {
      map.insert(std::make_pair(uniques[i], o[i]));
    }

    // grab min ranks
    p_data = Rcpp::internal::r_vector_start<STRSXP>(data);
    previous = *p_data++;

    int o_pos;
    orders[0] = o_pos = map.find(previous)->second;

    for (int i=1; i<n; ++i, ++p_data) {
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

    CharacterVector big = no_init(n);
    CharacterVector::iterator it = big.begin();
    std::copy(left.begin(), left.end(), it);
    std::copy(right.begin(), right.end(), it + nleft);
    return Language("unique", big).fast_eval();
  }

}
