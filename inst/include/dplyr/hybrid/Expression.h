#ifndef dplyr_hybrid_expression_h
#define dplyr_hybrid_expression_h

#include <tools/SymbolString.h>
#include <tools/SymbolMap.h>

#include <dplyr/hybrid/Column.h>

namespace dplyr {
namespace hybrid {

template <typename LazySubsets>
class Expression {
public:

  typedef std::pair<bool, SEXP> ArgPair;

  Expression(SEXP expr_, const LazySubsets& subsets_) :
    expr(expr_),
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
      func = CADR(head);
      package = CADDR(head);
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

  inline bool is_fun(SEXP symbol, SEXP pkg) {
    return valid && symbol == func && (package == R_NilValue || package == pkg);
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

  SEXP func;
  SEXP package;
  bool valid;

  const LazySubsets& subsets;

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
