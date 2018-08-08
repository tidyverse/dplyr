#ifndef dplyr_hybrid_expression_h
#define dplyr_hybrid_expression_h

#include <tools/SymbolString.h>
#include <tools/SymbolMap.h>

#include <dplyr/hybrid/Column.h>

namespace dplyr {
namespace hybrid {

struct FindFunData {
  const SEXP symbol;
  const SEXP env;
  SEXP rho;

  FindFunData(SEXP symbol_, SEXP env_) :
    symbol(symbol_),
    env(env_),
    rho(R_NilValue)
  {}

  inline Rboolean findFun() {
    return R_ToplevelExec(protected_findFun, reinterpret_cast<void*>(this));
  }

  static void protected_findFun(void* data) {
    FindFunData* find_data = reinterpret_cast<FindFunData*>(data);
    find_data->protected_findFun();
  }

  inline void protected_findFun() {
    rho = env;

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

template <typename LazySubsets>
class Expression {
public:

  typedef std::pair<bool, SEXP> ArgPair;

  Expression(SEXP expr_, const LazySubsets& subsets_, SEXP env_) :
    expr(expr_),
    env(env_),
    func(R_NilValue),
    package(R_NilValue),
    valid(false),
    subsets(subsets_),
    n(0)
  {
    // the function called, e.g. n, or dplyr::n
    SEXP head = CAR(expr);
    if (TYPEOF(head) == SYMSXP) {
      valid = true;
      func = head;
    } else if (TYPEOF(head) == LANGSXP && Rf_length(head) == 3 && CAR(head) == R_DoubleColonSymbol && TYPEOF(CADR(head)) == SYMSXP && TYPEOF(CADDR(head)) == SYMSXP) {
      valid = true;
      func = CADDR(head);
      package = CADR(head);
    }

    // the arguments
    for (SEXP p = CDR(expr); !Rf_isNull(p); p = CDR(p)) {
      n++;
      values.push_back(CAR(p));
      tags.push_back(TAG(p));
    }
  }

  inline SEXP value(int i) {
    return values[i];
  }

  inline int size() const {
    return n;
  }

  inline bool is_fun(SEXP symbol, SEXP pkg, SEXP ns) {
    // quickly escape if this has no chance to be the function we look for
    if (!valid || symbol != func) {
      return false;
    }
    if (package == R_NilValue) {
      // bare expression, e.g. n() so we need to check that `n` evaluates to the
      // function in the right environment, otherwise we let R evaluate the call
      FindFunData finder(symbol, env);
      if(!finder.findFun()) return false;

      return finder.rho == ns;
    } else {
      // expression of the form pkg::fun so check that pkg is the correct one
      return package == pkg;
    }
  }

  inline bool is_named(int i, SEXP symbol) const {
    return tags[i] == symbol;
  }

  inline bool is_unnamed(int i) const {
    return Rf_isNull(tags[i]);
  }

  inline bool is_scalar_logical(int i, bool& test) const {
    SEXP val = values[i];
    bool res = TYPEOF(val) == LGLSXP && Rf_length(val) == 1 ;
    if (res) {
      test = LOGICAL(val)[0];
    }
    return res;
  }

  inline bool is_scalar_int(int i, int& out) const {
    SEXP val = values[i];
    if (Rf_length(val) != 1) return false;
    switch (TYPEOF(val)) {
    case INTSXP:
      out = INTEGER(val)[0];
      return true;
    case REALSXP:
      out = Rcpp::internal::r_coerce<REALSXP, INTSXP>(REAL(val)[0]);
      return true;
    default:
      break;
    }
    return false;
  }

  inline bool is_column(int i, Column& column) const {
    SEXP val = values[i];

    // when val is a quosure, grab its expression
    //
    // this allows for things like mean(!!quo(x)) or mean(!!quo(!!sym("x")))
    // to go through hybrid evaluation
    if (internal::rlang_api().is_quosure(val)) {
      val = internal::rlang_api().quo_get_expr(val);
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

  const LazySubsets& subsets;

  int n;
  std::vector<SEXP> values;
  std::vector<SEXP> tags;

  Environment ns_base;
  Environment ns_dplyr;
  Environment ns_stats;

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
    bool test = subsets.has_variable(symbol);
    if (test) {
      // only treat very simple columns as columns, leave other to R
      SEXP data = subsets.get_variable(symbol);
      if (Rf_isObject(data) || Rf_isS4(data) || RCPP_GET_CLASS(data) != R_NilValue) return false;

      column.data = data;
      column.is_summary = subsets.is_summary(symbol);
      column.is_desc = desc;
    }
    return test;
  }


};

}
}

#endif
