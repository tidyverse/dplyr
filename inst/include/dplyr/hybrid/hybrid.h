#ifndef dplyr_hybrid_hybrid_h
#define dplyr_hybrid_hybrid_h

#include <tools/Quosure.h>

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

template <typename SlicedTibble, typename Operation>
SEXP hybrid_do(SEXP expr, const SlicedTibble& data, const DataMask<SlicedTibble>& mask, SEXP env, const Operation& op) {
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

  Environment ns_base  = Environment::base_env();
  Environment ns_dplyr = Environment::namespace_env("dplyr");
  Environment ns_stats = Environment::namespace_env("stats");

  Expression<SlicedTibble> expression(expr, mask, env);
  if (!expression.is_valid()) return R_UnboundValue;

  // functions that take variadic number of arguments
  if (expression.is_fun(s_n_distinct, s_dplyr, ns_dplyr)) {
    return n_distinct_(data, expression, op);
  }

  // fixed sized expressions
  switch (expression.size()) {
  case 0:
    // n()
    if (expression.is_fun(s_n, s_dplyr, ns_dplyr)) {
      return op(n_(data));
    }

    // group_indices()
    if (expression.is_fun(s_group_indices, s_dplyr, ns_dplyr)) {
      return op(group_indices_(data));
    }

    // row_number()
    if (expression.is_fun(s_row_number, s_dplyr, ns_dplyr)) {
      return op(row_number_(data));
    }

    break;

  case 1:
    if (expression.is_fun(s_sum, s_base, ns_base)) {
      // sum( <column> ) and base::sum( <column> )
      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return sum_(data, x, /* na.rm = */ false, op);
      }
    } else if (expression.is_fun(s_mean, s_base, ns_base)) {
      // mean( <column> ) and base::mean( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return mean_(data, x, false, op);
      }
    } else if (expression.is_fun(s_var, s_stats, ns_stats)) {
      // var( <column> ) and stats::var( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return var_(data, x, false, op);
      }
    } else if (expression.is_fun(s_sd, s_stats, ns_stats)) {
      // sd( <column> ) and stats::sd( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return sd_(data, x, false, op);
      }
    } else if (expression.is_fun(s_first, s_dplyr, ns_dplyr)) {
      // first( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return first1_(data, x, op);
      }
    } else if (expression.is_fun(s_last, s_dplyr, ns_dplyr)) {
      // last( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return last1_(data, x, op);
      }
    } else if (expression.is_fun(s_min, s_base, ns_base)) {
      // min( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return min_(data, x, false, op);
      }
    } else if (expression.is_fun(s_max, s_base, ns_base)) {
      // max( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return max_(data, x, false, op);
      }
    } else if (expression.is_fun(s_row_number, s_dplyr, ns_dplyr)) {
      // row_number( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return row_number_1(data, x, op);
      }
    } else if (expression.is_fun(s_ntile, s_dplyr, ns_dplyr)) {
      // ntile( n = <int> )

      int n;
      if (expression.is_named(0, s_n) && expression.is_scalar_int(0, n)) {
        return op(ntile_1(data, n));
      }
    } else if (expression.is_fun(s_min_rank, s_dplyr, ns_dplyr)) {
      // min_rank( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return min_rank_(data, x, op);
      }
    } else if (expression.is_fun(s_percent_rank, s_dplyr, ns_dplyr)) {
      // percent_rank( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return percent_rank_(data, x, op);
      }
    } else if (expression.is_fun(s_dense_rank, s_dplyr, ns_dplyr)) {
      // dense_rank( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return dense_rank_(data, x, op);
      }
    } else if (expression.is_fun(s_cume_dist, s_dplyr, ns_dplyr)) {
      // cume_dist( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return cume_dist_(data, x, op);
      }
    } else if (expression.is_fun(s_lead, s_dplyr, ns_dplyr)) {
      // lead( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return lead_1(data, x, 1, op);
      }
    } else if (expression.is_fun(s_lag, s_dplyr, ns_dplyr)) {
      // lag( <column> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x)) {
        return lag_1(data, x, 1, op);
      }
    }

    // give up:
    return R_UnboundValue;

  case 2:
    if (expression.is_fun(s_sum, s_base, ns_base)) {
      // sum( <column>, na.rm = <bool> )
      // base::sum( <column>, na.rm = <bool> )

      Column x;
      bool test;
      if (expression.is_unnamed(0) && expression.is_column(0, x) &&
          expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
         ) {
        return sum_(data, x, test, op);
      }
    } else if (expression.is_fun(s_mean, s_base, ns_base)) {
      // mean( <column>, na.rm = <bool> )
      // base::mean( <column>, na.rm = <bool> )
      Column x;
      bool test;

      if (expression.is_unnamed(0) && expression.is_column(0, x) &&
          expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
         ) {
        return mean_(data, x, test, op);
      }
    } else if (expression.is_fun(s_var, s_stats, ns_stats)) {
      // var( <column>, na.rm = <bool> )
      // stats::var( <column>, na.rm = <bool> )

      Column x;
      bool test;

      if (
        expression.is_unnamed(0) && expression.is_column(0, x) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
      ) {
        return var_(data, x, test, op);
      }

    } else if (expression.is_fun(s_sd, s_stats, ns_stats)) {
      // sd( <column>, na.rm = <bool> )
      // stats::sd( <column>, na.rm = <bool> )

      Column x;
      bool test;

      if (
        expression.is_unnamed(0) && expression.is_column(0, x) &&
        expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)
      ) {
        return sd_(data, x, test, op);
      }

    } else if (expression.is_fun(s_first, s_dplyr, ns_dplyr)) {
      // first( <column>, default = <scalar> )

      Column x;
      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_default)) {
        return first2_(data, x, /* default = */ expression.value(1), op);
      }

    } else if (expression.is_fun(s_last, s_dplyr, ns_dplyr)) {
      // last( <column>, default = <scalar> )

      Column x;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_default)) {
        return last2_(data, x, /* default = */ expression.value(1), op);
      }

    } else if (expression.is_fun(s_nth, s_dplyr, ns_dplyr)) {
      // nth( <column>, n = <int> )

      Column x;
      int n;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n)) {
        return nth2_(data, x, n, op);
      }

    } else if (expression.is_fun(s_min, s_base, ns_base)) {
      // min( <column>, na.rm = <bool> )

      Column x;
      bool test;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)) {
        return min_(data, x, /* na.rm = */ test, op);
      }

    } else if (expression.is_fun(s_max, s_base, ns_base)) {
      // max( <column>, na.rm = <bool> )

      Column x;
      bool test;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_narm) && expression.is_scalar_logical(1, test)) {
        return max_(data, x, /* na.rm = */ test, op);
      }

    } else if (expression.is_fun(s_ntile, s_dplyr, ns_dplyr)) {
      // ntile( <column>, n = <int> )

      Column x;
      int n;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n)) {
        return ntile_2(data, x, n, op);
      } else {
        return R_UnboundValue;
      }
    } else if (expression.is_fun(s_lead, s_dplyr, ns_dplyr)) {
      // lead( <column>, n = <int> )

      Column x;
      int n;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n) && n >= 0) {
        return lead_1(data, x, n, op);
      }

    } else if (expression.is_fun(s_lag, s_dplyr, ns_dplyr)) {
      // lag( <column>, n = <int> )

      Column x;
      int n;

      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n) && n >= 0) {
        return lag_1(data, x, n, op);
      }

    } else if (expression.is_fun(s_in, s_base, ns_base)) {
      // <column> %in% <column>

      Column lhs;
      Column rhs;

      if (expression.is_unnamed(0) && expression.is_column(0, lhs) && expression.is_unnamed(1) && expression.is_column(1, rhs)) {
        return in_column_column(data, lhs, rhs, op);
      }

    }

    // give up
    return R_UnboundValue;

  case 3:

    // nth( <column>, n = <int>, default = <scalar> )
    if (expression.is_fun(s_nth, s_dplyr, ns_dplyr)) {
      Column x;
      int n;
      if (expression.is_unnamed(0) && expression.is_column(0, x) && expression.is_named(1, s_n) && expression.is_scalar_int(1, n) && expression.is_named(2, s_default)) {
        return nth3_default(data, x, n, expression.value(2), op);
      }
    }

    // give up
    return R_UnboundValue;

  default:
    break;
  }

  // give up
  return R_UnboundValue;
}

template <typename SlicedTibble>
SEXP summarise(const NamedQuosure& quosure, const SlicedTibble& data, const DataMask<SlicedTibble>& mask) {
  return hybrid_do(quosure.expr(), data, mask, quosure.env(), Summary());
}

template <typename SlicedTibble>
SEXP window(SEXP expr, const SlicedTibble& data, const DataMask<SlicedTibble>& mask, SEXP env) {
  return hybrid_do(expr, data, mask, env, Window());
}

template <typename SlicedTibble>
SEXP match(SEXP expr, const SlicedTibble& data, const DataMask<SlicedTibble>& mask, SEXP env) {
  bool test = !is_vector(expr);
  RObject klass;
  if (test) {
    klass = (hybrid_do(expr, data, mask, env, Match()));
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
