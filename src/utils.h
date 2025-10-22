#ifndef DPLYR_UTILS_H
#define DPLYR_UTILS_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

static inline
bool str_is_utf8(SEXP x) {
#if (R_VERSION >= R_Version(4, 5, 0))
  return Rf_charIsUTF8(x);
#else
  const int mask_ascii = 8;
  const int mask_utf8 = 64;
  const int levels = LEVELS(x);
  return (levels & mask_ascii) || (levels & mask_utf8);
#endif
}

static inline
SEXP str_as_utf8(SEXP x) {
  if (str_is_utf8(x)) {
    return x;
  } else {
    return Rf_mkCharCE(Rf_translateCharUTF8(x), CE_UTF8);
  }
}

#endif
