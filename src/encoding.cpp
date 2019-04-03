#include "pch.h"
#include <dplyr/main.h>

#include <tools/encoding.h>
#include <tools/utils.h>

namespace dplyr {

R_xlen_t get_first_reencode_pos(const Rcpp::CharacterVector& x) {
  R_xlen_t len = x.length();
  for (R_xlen_t i = 0; i < len; ++i) {
    SEXP xi = x[i];
    if (xi != NA_STRING && !IS_ASCII(xi) && !IS_UTF8(xi)) {
      return i;
    }
  }

  return len;
}

Rcpp::CharacterVector reencode_char(SEXP x) {
  if (Rf_isFactor(x)) return reencode_factor(x);

#if (defined(R_VERSION) && R_VERSION >= R_Version(3, 5, 0))
  // If ret is an Altrep call DATAPTR to materialize it fully here, since we
  // will be touching all the elements anyway.
  if (ALTREP(x)) {
    DATAPTR(x);
  }
#endif

  Rcpp::CharacterVector ret(x);
  R_xlen_t first = get_first_reencode_pos(ret);
  if (first >= ret.length()) return ret;

  ret = clone(ret);

  R_xlen_t len = ret.length();
  for (R_xlen_t i = first; i < len; ++i) {
    SEXP reti = ret[i];
    if (reti != NA_STRING && !IS_ASCII(reti) && !IS_UTF8(reti)) {
      ret[i] = Rcpp::String(Rf_translateCharUTF8(reti), CE_UTF8);
    }
  }

  return ret;
}

Rcpp::CharacterVector reencode_factor(Rcpp::IntegerVector x) {
  Rcpp::CharacterVector levels(reencode_char(get_levels(x)));
  Rcpp::CharacterVector ret(x.length());

  R_xlen_t nlevels = levels.length();

  R_xlen_t len = x.length();
  for (R_xlen_t i = 0; i < len; ++i) {
    int xi = x[i];
    if (xi <= 0 || xi > nlevels)
      ret[i] = NA_STRING;
    else
      ret[i] = levels[xi - 1];
  }

  return ret;
}

}
