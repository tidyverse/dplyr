#include "dplyr.h"

#define DPLYR_REP_INT_OUT_SIZE(CTYPE, DEREF, ELT_INVALID) do { \
  CTYPE* p_times = DEREF(times);                               \
                                                               \
  for (R_xlen_t i = 0; i < size_times; ++i) {                  \
    CTYPE elt = p_times[i];                                    \
                                                               \
    if (ELT_INVALID) {                                         \
      Rf_errorcall(R_NilValue, "Invalid `times` value");       \
    }                                                          \
                                                               \
    size_out_dbl += elt;                                       \
  }                                                            \
} while(0)

#define DPLYR_REP_INT_LOOP(CTYPE, DEREF) do {                  \
  CTYPE* p_times = DEREF(times);                               \
                                                               \
  for (R_xlen_t i = 0; i < size_times; ++i) {                  \
    int x_elt = p_x[i];                                        \
    R_xlen_t times_elt = (R_xlen_t) p_times[i];                \
                                                               \
    for (R_xlen_t j = 0; j < times_elt; ++j, ++k) {            \
      p_out[k] = x_elt;                                        \
    }                                                          \
  }                                                            \
} while(0)

/*
 * Repeat an integer vector a number of times
 *
 * The purpose of this helper is as a faster version of `rep.int()`. It is
 * very slow in R 3.6 and below, because it repeatedly accesses elements of
 * each array when it doesn't need to.
 *
 * @param x An integer vector to repeat.
 * @param times Generally, an integer vector of times to repeat each element
 *   of `x`. Can be a double vector to generate long vectors.
 *
 * The size of `times` must match the size of `x`. Neither are recycled.
 */
SEXP dplyr_rep_int(SEXP x, SEXP times) {
  if (TYPEOF(x) != INTSXP) {
    Rf_errorcall(R_NilValue, "`x` must be an integer vector");
  }

  SEXPTYPE type_times = TYPEOF(times);

  // Allow double `times` for long vectors
  switch (type_times) {
  case INTSXP: break;
#ifdef LONG_VECTOR_SUPPORT
  case REALSXP: break;
#endif
  default:
    Rf_errorcall(
      R_NilValue,
      "`times` has unknown type %s", Rf_type2char(TYPEOF(times))
    );
  }

  R_xlen_t size_times = Rf_xlength(times);

  // For internal use we don't allow any recycling
  if (size_times != Rf_xlength(x)) {
    Rf_errorcall(R_NilValue, "Internal error: `x` and `times` should have equal lengths");
  }

  // Hold the output size in a double at first, so we can check if we go
  // above the maximum xlen size with less chance of overflow
  double size_out_dbl = 0;

  switch (type_times) {
  case INTSXP: DPLYR_REP_INT_OUT_SIZE(int, INTEGER, elt < 0); break;
  case REALSXP: DPLYR_REP_INT_OUT_SIZE(double, REAL, ISNAN(elt) || elt < 0 || elt > R_XLEN_T_MAX); break;
  default: Rf_errorcall(R_NilValue, "Internal error: Should never get here");
  }

  if (size_out_dbl > R_XLEN_T_MAX) {
    Rf_errorcall(
      R_NilValue,
      "Invalid `times`. Total number of `times` is greater than "
      "the maximum allowed vector length."
    );
  }

  R_xlen_t size_out = (R_xlen_t) size_out_dbl;

  const int* p_x = INTEGER(x);

  SEXP out = PROTECT(Rf_allocVector(INTSXP, size_out));
  int* p_out = INTEGER(out);

  // Keep track of element index in `out`
  R_xlen_t k = 0;

  switch (type_times) {
  case INTSXP: DPLYR_REP_INT_LOOP(int, INTEGER); break;
  case REALSXP: DPLYR_REP_INT_LOOP(double, REAL); break;
  default: Rf_errorcall(R_NilValue, "Internal error: Should never get here");
  }

  UNPROTECT(1);
  return out;
}

#undef DPLYR_REP_INT_OUT_SIZE
#undef DPLYR_REP_INT_LOOP
