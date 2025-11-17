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

#endif
