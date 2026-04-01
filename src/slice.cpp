#include "dplyr.h"
#include "utils.h"

SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private) {
  DPLYR_MASK_INIT();

  // Ensure we pass a quosure, which forces `rlang::eval_tidy()` to extract the
  // environment from the quosure rather than using `env`, so what we pass as
  // `env` doesn't matter.
  check_quosure(quo);
  SEXP env = R_EmptyEnv;

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));

  for (R_xlen_t i = 0; i < ngroups; i++) {
    DPLYR_MASK_ITERATION_INIT();
    DPLYR_MASK_SET_GROUP(i);
    SET_VECTOR_ELT(chunks, i, DPLYR_MASK_EVAL(quo, env));
    DPLYR_MASK_ITERATION_FINALISE();
  }

  UNPROTECT(1);
  DPLYR_MASK_FINALISE();

  return chunks;
}
