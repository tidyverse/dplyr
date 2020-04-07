#include "dplyr.h"

SEXP dplyr_promises_args(SEXP names_bindings, SEXP promise_fn) {
  PROTECT_INDEX ipx;
  SEXP args = R_NilValue;
  PROTECT_WITH_INDEX(args, &ipx);

  for (R_len_t n = LENGTH(names_bindings); n > 0; n--) {
    SEXP i = PROTECT(Rf_ScalarInteger(n));
    SEXP call = PROTECT(Rf_lang2(promise_fn, i));
    REPROTECT(args = Rf_cons(call, args), ipx);
    SET_TAG(args, Rf_installChar(STRING_ELT(names_bindings, n-1)));
    UNPROTECT(2);
  }
  UNPROTECT(1);
  return args;
}
