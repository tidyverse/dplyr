#ifndef dplyr_hybrid_hybrid_h
#define dplyr_hybrid_hybrid_h

#include <tools/SymbolString.h>
#include <dplyr/hybrid/Dispatch.h>
#include <dplyr/hybrid/HybridVectorScalarResult.h>

#include <dplyr/hybrid/scalar_result/Count.h>
#include <dplyr/hybrid/scalar_result/sum.h>

#include <tools/SymbolMap.h>

namespace dplyr{
namespace hybrid{

template <typename LazySubsets>
class Expression {
public:

  typedef std::pair<bool,SEXP> ArgPair;

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
    if (TYPEOF(head) == SYMSXP){
      valid = true;
      func = head;
    } else if (TYPEOF(head) == LANGSXP && Rf_length(head) == 3 && CAR(head) == R_DoubleColonSymbol && TYPEOF(CADR(head)) == SYMSXP && TYPEOF(CADDR(head)) == SYMSXP ){
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

  inline int size() const{
    return n;
  }

  inline bool is_fun(SEXP symbol, SEXP pkg){
    return valid && symbol == func && (package == R_NilValue || package == pkg);
  }

  inline bool is_named(int i, SEXP symbol) const {
    return tags[i] == symbol;
  }

  inline bool is_unnamed(int i) const {
    return Rf_isNull(tags[i]);
  }

  inline bool is_scalar_logical(int i) const {
    SEXP val = values[i];
    return TYPEOF(val) == LGLSXP && Rf_length(val) == 1 ;
  }

  inline bool is_column(int i, SEXP& column) const {
    SEXP val = values[i];

    if (TYPEOF(val) == SYMSXP){
      return test_is_column(val, column);
    }

    if (TYPEOF(val) == LANGSXP && Rf_length(val) == 3 && CADR(val) == Rf_install(".data")){
      SEXP fun = CAR(val);
      SEXP rhs = CADDR(val);

      if (fun == R_DollarSymbol) {
        // .data$x
        if (TYPEOF(rhs) == SYMSXP) return test_is_column(rhs, column);

        // .data$"x"
        if (TYPEOF(rhs) == STRSXP && Rf_length(rhs) == 1) return test_is_column(Rf_installChar(STRING_ELT(rhs, 0)), column);
      } else if (fun == R_Bracket2Symbol) {
        // .data[["x"]]
        if (TYPEOF(rhs) == STRSXP && Rf_length(rhs) == 1) return test_is_column(Rf_installChar(STRING_ELT(rhs, 0)), column);
      }
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

  inline bool test_is_column(Rcpp::Symbol s, SEXP& column) const {
    SymbolString symbol(s);
    bool test = subsets.has_variable(symbol);
    if (test) {
      column = subsets.get_variable(symbol);
    }
    return test;
  }


};

template <typename SlicedTibble, typename LazySubsets, typename Operation>
SEXP hybrid_do(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env, const Operation& op){
  if (TYPEOF(expr) != LANGSXP) return R_UnboundValue;

  static SEXP s_n = Rf_install("n");
  static SEXP s_sum = Rf_install("sum");

  static SEXP s_dplyr = Rf_install("dplyr");
  static SEXP s_base = Rf_install("base");

  Expression<LazySubsets> expression(expr, subsets);
  switch(expression.size()){
  case 0:
    // n()
    if (expression.is_fun(s_n, s_dplyr)) {
      return op(dplyr::hybrid::Count<SlicedTibble>(data));
    }
    break;

  case 1:
    // sum( <column> )
    SEXP column ;
    if (expression.is_fun(s_sum, s_base) && expression.is_unnamed(0) && expression.is_column(0, column)) {
      return op(dplyr::hybrid::SumDispatch<SlicedTibble>(data, column, false));
    }

  default:
    break;
  }

  // give up
  return R_UnboundValue;
}

template <typename SlicedTibble, typename LazySubsets>
SEXP summarise(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env){
  return hybrid_do(expr, data, subsets, env, Summary());
}

template <typename SlicedTibble, typename LazySubsets>
SEXP window(SEXP expr, const SlicedTibble& data, const LazySubsets& subsets, SEXP env){
  return hybrid_do(expr, data, subsets, env, Window());
}


}
}

#endif
