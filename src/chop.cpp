#include "dplyr.h"

SEXP dplyr_lazy_vec_chop(SEXP e, SEXP data, SEXP indices) {
  SEXP names = PROTECT(Rf_getAttrib(data, R_NamesSymbol));
  R_xlen_t n = XLENGTH(data);

  for (R_xlen_t i = 0; i < n; i++) {
    SEXP prom = PROTECT(Rf_allocSExp(PROMSXP));
    SET_PRENV(prom, R_EmptyEnv);
    SET_PRCODE(prom, Rf_lang3(dplyr::functions::vec_chop, VECTOR_ELT(data, i), indices));
    SET_PRVALUE(prom, R_UnboundValue);

    Rf_defineVar(Rf_installChar(STRING_ELT(names, i)), prom, e);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return e;
}
