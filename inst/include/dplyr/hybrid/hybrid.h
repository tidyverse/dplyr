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

#include <dplyr/symbols.h>

namespace dplyr {
namespace hybrid {

template <typename SlicedTibble, typename Operation>
SEXP hybrid_do(SEXP expr, const SlicedTibble& data, const DataMask<SlicedTibble>& mask, SEXP env, const Operation& op) {
  if (TYPEOF(expr) != LANGSXP) return R_UnboundValue;

  Expression<SlicedTibble> expression(expr, mask, env);
  switch (expression.get_id()) {
  case N_DISTINCT:
    return n_distinct_(data, expression, op);
  case N:
    return expression.size() == 0 ? op(n_(data)) : R_UnboundValue;
  case GROUP_INDICES:
    return expression.size() == 0 ? op(group_indices_(data)) : R_UnboundValue;
  case ROW_NUMBER:
    return row_number_dispatch(data, expression, op);

  case SUM:
    return sum_dispatch(data, expression, op);
  case MEAN:
    return mean_dispatch(data, expression, op);
  case VAR:
    return var_dispatch(data, expression, op);
  case SD:
    return sd_dispatch(data, expression, op);

  case FIRST:
    return first_dispatch(data, expression, op);
  case LAST:
    return last_dispatch(data, expression, op);
  case NTH:
    return nth_dispatch(data, expression, op);

  case MIN:
    return min_(data, expression, op);
  case MAX:
    return max_(data, expression, op);

  case NTILE:
    return ntile_dispatch(data, expression, op);
  case MIN_RANK:
    return min_rank_dispatch(data, expression, op);
  case DENSE_RANK:
    return dense_rank_dispatch(data, expression, op);
  case PERCENT_RANK:
    return percent_rank_dispatch(data, expression, op);
  case CUME_DIST:
    return cume_dist_dispatch(data, expression, op);

  case LEAD:
    return lead_dispatch(data, expression, op);
  case LAG:
    return lag_dispatch(data, expression, op);

  case IN:
    return in_dispatch(data, expression, op);

  case NOMATCH:
    break;
  }
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
    Expression<SlicedTibble> expression(expr, mask, env);
    res.attr("fun") = Rf_ScalarString(PRINTNAME(expression.get_fun()));
    res.attr("package") = Rf_ScalarString(PRINTNAME(expression.get_package()));
    res.attr("cpp_class") = klass;

    SEXP call = PROTECT(Rf_duplicate(expr));
    SETCAR(call, Rf_lang3(symbols::double_colon, expression.get_package(), expression.get_fun()));
    res.attr("call") = call;
    UNPROTECT(1);
  }
  return res;
}

}
}

#endif
