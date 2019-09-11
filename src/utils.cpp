#include <Rcpp.h>

#include <dplyr/rlang.h>
#include <dplyr/symbols.h>

// [[Rcpp::export(rng = false)]]
bool quo_is_variable_reference(SEXP quo) {
  SEXP expr = CADR(quo);

  // ok if symbol
  if (TYPEOF(expr) == SYMSXP)
    return true;

  // is it using the .data pronoun instead ?
  if (TYPEOF(expr) != LANGSXP || Rf_length(expr) != 3)
    return false;

  SEXP first = CADR(expr);
  if (first != dplyr::symbols::dot_data)
    return false;

  SEXP second = CADDR(expr);
  SEXP fun = CAR(expr);

  // .data$x or .data$"x"
  if (fun == R_DollarSymbol && (TYPEOF(second) == SYMSXP || TYPEOF(second) == STRSXP))
    return true;

  // .data[["x"]]
  if (fun == R_Bracket2Symbol && TYPEOF(second) == STRSXP)
    return true;

  return false;
}
