#ifndef DPLYR_UTILS_H
#define DPLYR_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

// String encoding normalization
// From https://github.com/r-lib/vctrs/pull/2085
static inline bool string_is_ascii_or_utf8(SEXP x) {
#if (R_VERSION >= R_Version(4, 5, 0))
  return Rf_charIsASCII(x) || (Rf_getCharCE(x) == CE_UTF8) || (x == NA_STRING);
#else
  const int mask_ascii = 8;
  const int mask_utf8 = 64;
  const int levels = LEVELS(x);
  return (levels & mask_ascii) || (levels & mask_utf8) || (x == NA_STRING);
#endif
}

static inline SEXP string_as_utf8(SEXP x) {
  return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
}

// rlang's C level `is_quosure()`
// https://github.com/r-lib/rlang/blob/01c296faa170d874cb8a3971e9cf7e5cd384b27b/src/internal/quo.c#L17
static inline bool is_quosure(SEXP x) {
  return TYPEOF(x) == LANGSXP && Rf_inherits(x, "quosure");
}

static inline void check_quosure(SEXP x) {
  if (!is_quosure(x)) {
    Rf_errorcall(R_NilValue, "Internal error: `x` must be a quosure.");
  }
}

static inline void check_list_of_quosures(SEXP x) {
  const SEXP* v_x = VECTOR_PTR_RO(x);
  const R_xlen_t size = Rf_xlength(x);

  for (R_xlen_t i = 0; i < size; ++i) {
    if (!is_quosure(v_x[i])) {
      Rf_errorcall(R_NilValue, "Internal error: `x[[%i]]` must be a quosure.", (int) i + 1);
    }
  }
}

#endif
