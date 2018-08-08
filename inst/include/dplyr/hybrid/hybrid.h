#ifndef dplyr_hybrid_hybrid_h
#define dplyr_hybrid_hybrid_h

#include <dplyr/hybrid/Expression.h>

#include <dplyr/hybrid/scalar_result/n.h>
#include <dplyr/hybrid/scalar_result/sum.h>
#include <dplyr/hybrid/scalar_result/mean_sd_var.h>
#include <dplyr/hybrid/scalar_result/n_distinct.h>
#include <dplyr/hybrid/scalar_result/first_last.h>
#include <dplyr/hybrid/scalar_result/group_indices.h>
#include <dplyr/hybrid/scalar_result/min_max.h>

#include <dplyr/hybrid/vector_result/row_number.h>
#include <dplyr/hybrid/vector_result/ntile.h>
#include <dplyr/hybrid/vector_result/rank.h>
#include <dplyr/hybrid/vector_result/lead_lag.h>
#include <dplyr/hybrid/vector_result/in.h>

namespace dplyr {
namespace hybrid {

template <typename SlicedTibble, typename LazySubsets, typename Operation>
SEXP hybrid_do(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env, const Operation& op) {
  if (TYPEOF(expr) != LANGSXP) return R_UnboundValue;

  static SEXP s_n = Rf_install("n");
  static SEXP s_sum = Rf_install("sum");
  static SEXP s_mean = Rf_install("mean");
  static SEXP s_var = Rf_install("var");
  static SEXP s_sd = Rf_install("sd");
  static SEXP s_n_distinct = Rf_install("n_distinct");
  static SEXP s_first = Rf_install("first");
  static SEXP s_last = Rf_install("last");
  static SEXP s_nth = Rf_install("nth");
  static SEXP s_group_indices = Rf_install("group_indices");
  static SEXP s_min = Rf_install("min");
  static SEXP s_max = Rf_install("max");
  static SEXP s_row_number = Rf_install("row_number");
  static SEXP s_ntile = Rf_install("ntile");
  static SEXP s_min_rank = Rf_install("min_rank");
  static SEXP s_percent_rank = Rf_install("percent_rank");
  static SEXP s_dense_rank = Rf_install("dense_rank");
  static SEXP s_cume_dist = Rf_install("cume_dist");
  static SEXP s_lead = Rf_install("lead");
  static SEXP s_lag = Rf_install("lag");
  static SEXP s_in = Rf_install("%in%");

  static SEXP s_narm = Rf_install("na.rm");
  static SEXP s_default = Rf_install("default");

  static SEXP s_dplyr = Rf_install("dplyr");
  static SEXP s_base = Rf_install("base");
  static SEXP s_stats = Rf_install("stats");

  Expression<LazySubsets> expression(expr, subsets);

  Column column;
  Column column2;
  bool test;
  int n;

  // fixed sized expressions
  switch (expression.size()) {
  case 0:
    // n()
    if (expression.is_fun(s_n, s_dplyr)) {
      return op(n_(data));
    }

    // group_indices()
    if (expression.is_fun(s_group_indices, s_dplyr)) {
      return op(group_indices_(data));
    }

    // row_number()
    if (expression.is_fun(s_row_number, s_dplyr)) {
      return op(row_number_(data));
    }

    break;

  case 1:
    // sum( <column> ) and base::sum( <column> )
    if (expression.is_fun(s_sum, s_base) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return sum_(data, column, false, op);
    }

    // mean( <column> ) and base::mean( <column> )
    if (expression.is_fun(s_mean, s_base) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return mean_(data, column, false, op);
    }

    // var( <column> ) and stats::var( <column> )
    if (expression.is_fun(s_var, s_stats) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return var_(data, column, false, op);
    }

    // sd( <column> ) and stats::sd( <column> )
    if (expression.is_fun(s_sd, s_stats) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return sd_(data, column, false, op);
    }

    // first( <column> )
    if (expression.is_fun(s_first, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return first1_(data, column, op);
    }

    // last( <column> )
    if (expression.is_fun(s_last, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return last1_(data, column, op);
    }

    // min( <column> )
    if (expression.is_fun(s_min, s_base) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return min_(data, column, false, op);
    }

    // max( <column> )
    if (expression.is_fun(s_max, s_base) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return max_(data, column, false, op);
    }

    // row_number( <column> )
    if (expression.is_fun(s_row_number, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return row_number_1(data, column, op);
    }

    // ntile( n = <int> )
    if (expression.is_fun(s_ntile, s_dplyr) && expression.is_named(0, s_n) && expression.is_scalar_int(0, n)) {
      return op(ntile_1(data, n));
    }

    // min_rank( <column> )
    if (expression.is_fun(s_min_rank, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return min_rank_(data, column, op);
    }

    // percent_rank( <column> )
    if (expression.is_fun(s_percent_rank, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return percent_rank_(data, column, op);
    }

    // dense_rank( <column> )
    if (expression.is_fun(s_dense_rank, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return dense_rank_(data, column, op);
    }

    // cume_dist( <column> )
    if (expression.is_fun(s_cume_dist, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return cume_dist_(data, column, op);
    }

    // lead( <column> )
    if (expression.is_fun(s_lead, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return lead_1(data, column, 1, op);
    }

    // lag( <column> )
    if (expression.is_fun(s_lag, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return lag_1(data, column, 1, op);
    }

    break;

  case 2:
    // sum( <column>, na.rm = <bool> )
    // base::sum( <column>, na.rm = <bool> )
    if (expression.is_fun(s_sum, s_base) &&
        expression.is_unnamed(0) && expression.is_column(0, column) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
       ) {
      return sum_(data, column, test, op);
    }

    // mean( <column>, na.rm = <bool> )
    // base::mean( <column>, na.rm = <bool> )
    if (expression.is_fun(s_mean, s_base) &&
        expression.is_unnamed(0) && expression.is_column(0, column) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
       ) {
      return mean_(data, column, test, op);
    }

    // var( <column>, na.rm = <bool> )
    // stats::var( <column>, na.rm = <bool> )
    if (expression.is_fun(s_var, s_stats) &&
        expression.is_unnamed(0) && expression.is_column(0, column) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
       ) {
      return var_(data, column, test, op);
    }

    // sd( <column>, na.rm = <bool> )
    // stats::sd( <column>, na.rm = <bool> )
    if (expression.is_fun(s_sd, s_stats) &&
        expression.is_unnamed(0) && expression.is_column(0, column) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
       ) {
      return sd_(data, column, test, op);
    }

    // first( <column>, default = <scalar> )
    if (expression.is_fun(s_first, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_default)) {
      return first2_default(data, column, expression.value(1), op);
    }

    // last( <column>, default = <scalar> )
    if (expression.is_fun(s_last, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_default)) {
      return last2_default(data, column, expression.value(1), op);
    }

    // nth( <column>, n = <int> )
    if (expression.is_fun(s_nth, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_n)) {
      return nth2_(data, column, expression.value(1), op);
    }

    // min( <column>, na.rm = <bool> )
    if (expression.is_fun(s_min, s_base) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)) {
      return min_(data, column, test, op);
    }

    // max( <column>, na.rm = <bool> )
    if (expression.is_fun(s_max, s_base) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)) {
      return max_(data, column, test, op);
    }

    // ntile( <column>, n = <int> )
    if (expression.is_fun(s_ntile, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n)) {
      return ntile_2(data, column, n, op);
    }

    // lead( <column>, n = <int> )
    if (expression.is_fun(s_lead, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n)) {
      return lead_1(data, column, n, op);
    }

    // lag( <column>, n = <int> )
    if (expression.is_fun(s_lag, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n)) {
      return lag_1(data, column, n, op);
    }

    // <column> %in% <column>
    if (expression.is_fun(s_in, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_unnamed(1) && expression.is_column(1, column2)) {
      return in_column_column(data, column, column2, op);
    }

    break;

  case 3:

    // nth( <column>, n = <int>, default = <scalar> )
    if (expression.is_fun(s_nth, s_dplyr) && expression.is_unnamed(0) && expression.is_column(0, column) && expression.is_named(1, s_n) && expression.is_named(2, s_default)) {
      return nth3_default(data, column, expression.value(1), expression.value(2), op);
    }
    break;

  default:
    break;
  }

  // functions that take variadic number of arguments
  if (expression.is_fun(s_n_distinct, s_dplyr)) {
    return n_distinct_(data, expression, op);
  }

  // give up
  return R_UnboundValue;
}

template <typename SlicedTibble, typename LazySubsets>
SEXP summarise(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env) {
  return hybrid_do(expr, data, subsets, env, Summary());
}

template <typename SlicedTibble, typename LazySubsets>
SEXP window(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env) {
  return hybrid_do(expr, data, subsets, env, Window());
}

template <typename SlicedTibble, typename LazySubsets>
SEXP match(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env) {
  bool test = !is_vector(expr);
  RObject klass;
  if (test) {
    klass = (hybrid_do(expr, data, subsets, env, Match()));
    test = klass != R_UnboundValue;
  }
  LogicalVector res(1, test) ;
  res.attr("class") = "hybrid_call";
  res.attr("call") = expr;
  res.attr("env") = env;
  if (test) {
    res.attr("cpp_class") = klass;
  }
  return res;
}

}
}

#endif
