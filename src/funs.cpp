#include "dplyr.h"

SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right) {
  R_xlen_t n = XLENGTH(x);

  double left = REAL(s_left)[0], right = REAL(s_right)[0];
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  // Assume users know what they're doing with date/times. In the future
  // should ensure that left and right are the correct class too.
  if (!Rf_isNull(Rf_getAttrib(x, R_ClassSymbol)) && !Rf_inherits(x, "Date") && !Rf_inherits(x, "POSIXct")) {
    Rf_warningcall(R_NilValue, "between() called on numeric vector with S3 class");
  }

  if (R_IsNA(left) || R_IsNA(right)) {
    std::fill(LOGICAL(out), LOGICAL(out) + n, NA_LOGICAL);
  } else {
    int* p_out = LOGICAL(out);
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

  double sum = *p_out++ = *p_x;
  for (R_xlen_t i = 1; i < n; i++, ++p_x, ++p_out) {
    sum += *p_x;
    *p_out = sum / (i + 1.0);
  }

  UNPROTECT(1);
  return out;
}
