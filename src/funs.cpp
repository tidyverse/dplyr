#include "dplyr.h"

SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right) {
  R_xlen_t n = XLENGTH(x);

  double left = REAL(s_left)[0], right = REAL(s_right)[0];
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_out = LOGICAL(out);
  if (R_IsNA(left) || R_IsNA(right)) {
    for (R_xlen_t i=0; i<XLENGTH(out); i++, ++p_out) *p_out = NA_LOGICAL;

  } else {
    double* p_x = REAL(x);
    for (R_xlen_t i = 0; i < n; ++i, ++p_x, ++p_out) {
      *p_out = R_IsNA(*p_x) ? NA_LOGICAL : (*p_x >= left) && (*p_x <= right);
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP dplyr_cumall(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_x = LOGICAL(x);
  int* p_out = LOGICAL(out);

  // set out[i] to TRUE as long as x[i] is TRUE
  R_xlen_t i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == TRUE) {
      *p_out = TRUE;
    } else {
      break;
    }
  }
  if (i != n) {

    // set to NA as long as x[i] is NA or TRUE
    for (; i < n; i++, ++p_x, ++p_out) {
      if (*p_x == FALSE) {
        break;
      }
      *p_out = NA_LOGICAL;
    }

    // set remaining to FALSE
    if (i != n) {
      for (; i < n; i++, ++p_x, ++p_out) {
        *p_out = FALSE;
      }
    }

  }

  UNPROTECT(1);
  return out;
}

SEXP dplyr_cumany(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_x = LOGICAL(x);
  int* p_out = LOGICAL(out);

  // nothing to do as long as x[i] is FALSE
  R_xlen_t i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == FALSE) {
      *p_out = FALSE;
    } else {
      break;
    }
  }
  if (i < n) {
    // set to NA as long as x[i] is NA or FALSE
    for (; i < n; i++, ++p_x, ++p_out) {
      if (*p_x == TRUE) {
        break;
      }
      *p_out = NA_LOGICAL;
    }

    if (i < n) {
      // then if we are here, the rest is TRUE
      for (; i < n; i++, ++p_out) {
        *p_out = TRUE;
      }
    }

  }

  UNPROTECT(1);
  return out;

}

SEXP dplyr_cummean(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double* p_out = REAL(out);
  double* p_x = REAL(x);

  double sum = 0.0;
  for (R_xlen_t i = 0; i < n; i++, ++p_x, ++p_out) {
    sum += *p_x;
    *p_out = sum / (i + 1.0);
  }

  UNPROTECT(1);
  return out;
}
