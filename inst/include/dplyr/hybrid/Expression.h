#ifndef dplyr_hybrid_expression_h
#define dplyr_hybrid_expression_h

#include <dplyr/hybrid/Column.h>
#include <tools/SymbolString.h>
#include <dplyr/data/DataMask.h>

namespace dplyr {
namespace hybrid {

// When we do hybrid evaulation of fun(...) we need to make
// sure that fun is the function we want, and not masked
struct FindFunData {
  const SEXP symbol;
  const SEXP env;
  SEXP res;

  FindFunData(SEXP symbol_, SEXP env_) :
    symbol(symbol_),
    env(env_),
    res(R_NilValue)
  {}

  inline Rboolean findFun() {
    return R_ToplevelExec(protected_findFun, reinterpret_cast<void*>(this));
  }

  static void protected_findFun(void* data) {
    FindFunData* find_data = reinterpret_cast<FindFunData*>(data);
    find_data->protected_findFun();
  }

  inline void protected_findFun() {
    SEXP rho = env;

    while (rho != R_EmptyEnv) {
      SEXP vl = Rf_findVarInFrame3(rho, symbol, TRUE);

      if (vl != R_UnboundValue) {
        // a promise, we need to evaluate it to find out if it
        // is a function promise
        if (TYPEOF(vl) == PROMSXP) {
          PROTECT(vl);
          vl = Rf_eval(vl, rho);
          UNPROTECT(1);
        }

        // we found a function
        if (TYPEOF(vl) == CLOSXP || TYPEOF(vl) == BUILTINSXP || TYPEOF(vl) == SPECIALSXP) {
          res = vl;
          return;
        }

        // a missing, just let R evaluation work as we have no way to
        // assert if the missing argument would have evaluated to a function or data
        if (vl == R_MissingArg) {
          return;
        }
      }

      // go in the parent environment
      rho = ENCLOS(rho);
    }

    return;
  }
};

template <typename SlicedTibble>
class Expression {
public:
  typedef std::pair<bool, SEXP> ArgPair;

  Expression(SEXP expr_, const DataMask<SlicedTibble>& data_mask_, SEXP env_) :
    expr(expr_),
    env(env_),
    func(R_NilValue),
    package(R_NilValue),
    valid(false),
    data_mask(data_mask_),
    n(0)
  {
    static SEXP R_DoubleColonSymbol = Rf_install("::");
    static SEXP s_dplyr = Rf_install("dplyr");
    static SEXP s_stats = Rf_install("stats");
    static SEXP s_base  = Rf_install("base");

    // the function called, e.g. n, or dplyr::n
    SEXP head = CAR(expr);
    if (TYPEOF(head) == SYMSXP) {
      // a symbol
      valid = true;
      func = head;
    } else if (TYPEOF(head) == LANGSXP && Rf_length(head) == 3 && CAR(head) == R_DoubleColonSymbol && TYPEOF(CADR(head)) == SYMSXP && TYPEOF(CADDR(head)) == SYMSXP) {
      // a call of the `::` function
      func = CADDR(head);
      package = CADR(head);

      // give up on pkg::fun if pkg is not one of dplyr, stats or base
      // because we only hybrid functions from those
      valid = package == s_dplyr || package == s_stats || package == s_base;
    }

    // the arguments
    for (SEXP p = CDR(expr); !Rf_isNull(p); p = CDR(p)) {
      n++;
      values.push_back(CAR(p));
      tags.push_back(TAG(p));
    }
  }

  // the number of arguments in the call
  inline int size() const {
    return n;
  }

  // the constructor rules out some expressions
  inline bool is_valid() const {
    return valid;
  }

  // expression or value for the ith argument
  inline SEXP value(int i) const {
    return values[i];
  }

  // is this expression the function we are looking for
  inline bool is_fun(SEXP symbol, SEXP pkg, SEXP ns) {
    // quickly escape if this has no chance to be the function we look for
    if (symbol != func) {
      return false;
    }
    if (package == R_NilValue) {
      // bare expression, e.g. n() so we need to check that `n` evaluates to the
      // function in the right environment, otherwise we let R evaluate the call
      FindFunData finder(symbol, env);
      if (!finder.findFun()) return false;

      SEXP expected = Rf_findVarInFrame3(ns, symbol, TRUE);
      if (TYPEOF(expected) == PROMSXP) {
        PROTECT(expected);
        expected = Rf_eval(expected, ns);
        UNPROTECT(1);
      }

      return finder.res == expected;
    } else {
      // expression of the form pkg::fun so check that pkg is the correct one
      return package == pkg;
    }
  }

  // is the i-th argument called `symbol`
  inline bool is_named(int i, SEXP symbol) const {
    return tags[i] == symbol;
  }

  // is the i-th argument unnamed
  inline bool is_unnamed(int i) const {
    return Rf_isNull(tags[i]);
  }

  // is the ith argument a logical scalar
  inline bool is_scalar_logical(int i, bool& test) const {
    SEXP val = values[i];
    bool res = TYPEOF(val) == LGLSXP && Rf_length(val) == 1 ;
    if (res) {
      test = LOGICAL(val)[0];
    }
    return res;
  }

  // is the i-th argument a scalar int
  inline bool is_scalar_int(int i, int& out) const {
    SEXP val = values[i];
    bool unary_minus = false;

    // unary minus
    if (TYPEOF(val) == LANGSXP && Rf_length(val) == 2 && CAR(val) == Rf_install("-")) {
      val = CADR(val);
      unary_minus = true;
    }

    // symbol
    if (TYPEOF(val) == SYMSXP) {
      // reject if it's a column
      Column col;
      if (is_column(i, col)) {
        return false;
      }

      // keep trying if this the symbol is a binding in the .env
      val = Rf_findVarInFrame3(env, val, FALSE);
      if (val == R_UnboundValue) {
        return false;
      }
    }

    switch (TYPEOF(val)) {
    case INTSXP:
    {
      if (Rf_length(val) != 1) return false;
      int value = INTEGER(val)[0];
      if (IntegerVector::is_na(value)) {
        return false;
      }
      out = unary_minus ? -value : value;
      return true;
    }
    case REALSXP:
    {
      if (Rf_length(val) != 1) return false;
      int value = Rcpp::internal::r_coerce<REALSXP, INTSXP>(REAL(val)[0]);
      if (IntegerVector::is_na(value)) {
        return false;
      }
      out = unary_minus ? -value : value;
      return true;
    }
    default:
      break;
    }
    return false;
  }

  // is the ith argument a column
  inline bool is_column(int i, Column& column) const {
    SEXP val = values[i];

    // when val is a quosure, grab its expression
    //
    // this allows for things like mean(!!quo(x)) or mean(!!quo(!!sym("x")))
    // to go through hybrid evaluation
    if (rlang::is_quosure(val)) {
      val = rlang::quo_get_expr(val);
    }

    if (is_column_impl(val, column, false)) {
      return true;
    }
    if (TYPEOF(val) == LANGSXP && Rf_length(val) == 1 && CAR(val) == Rf_install("desc") && is_column_impl(CADR(val), column, true)) {
      return true;
    }
    return false;
  }

private:
  SEXP expr;
  SEXP env;

  SEXP func;
  SEXP package;
  bool valid;

  const DataMask<SlicedTibble>& data_mask;

  int n;
  std::vector<SEXP> values;
  std::vector<SEXP> tags;

  inline bool is_column_impl(SEXP val, Column& column, bool desc) const {
    if (TYPEOF(val) == SYMSXP) {
      return test_is_column(val, column, desc);
    }

    if (TYPEOF(val) == LANGSXP && Rf_length(val) == 3 && CADR(val) == Rf_install(".data")) {
      SEXP fun = CAR(val);
      SEXP rhs = CADDR(val);

      if (fun == R_DollarSymbol) {
        // .data$x
        if (TYPEOF(rhs) == SYMSXP) return test_is_column(rhs, column, desc);

        // .data$"x"
        if (TYPEOF(rhs) == STRSXP && Rf_length(rhs) == 1) return test_is_column(Rf_installChar(STRING_ELT(rhs, 0)), column, desc);
      } else if (fun == R_Bracket2Symbol) {
        // .data[["x"]]
        if (TYPEOF(rhs) == STRSXP && Rf_length(rhs) == 1) return test_is_column(Rf_installChar(STRING_ELT(rhs, 0)), column, desc);
      }
    }
    return false;
  }

  inline bool test_is_column(Rcpp::Symbol s, Column& column, bool desc) const {
    SymbolString symbol(s);
    // does the data mask have this symbol, and if so is it a real column (not a summarised)
    const ColumnBinding<SlicedTibble>* subset = data_mask.maybe_get_subset_binding(symbol);
    if (!subset || subset->is_summary()) return false;

    // only treat very simple columns as columns, leave other to R
    SEXP data = subset->get_data() ;
    if (Rf_isObject(data) || Rf_isS4(data) || RCPP_GET_CLASS(data) != R_NilValue) return false;

    column.data = data;
    column.is_desc = desc;
    return true;
  }


};

}
}

#endif
