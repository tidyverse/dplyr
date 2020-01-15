#include "dplyr.h"

SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private, SEXP env_context) {
  DPLYR_MASK_INIT();

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_SET_GROUP(i);
    SET_VECTOR_ELT(chunks, i, DPLYR_MASK_EVAL(quo));
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return chunks;
}
