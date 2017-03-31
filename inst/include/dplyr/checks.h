#ifndef dplyr_checks_H
#define dplyr_checks_H

#include <tools/SymbolString.h>

namespace dplyr {

enum SupportedType {
  DPLYR_LGLSXP = LGLSXP,
  DPLYR_INTSXP = INTSXP,
  DPLYR_REALSXP = REALSXP,
  DPLYR_CPLXSXP = CPLXSXP,
  DPLYR_STRSXP = STRSXP,
  DPLYR_VECSXP = VECSXP
};

inline std::string type_name(SEXP x) {
  switch (TYPEOF(x)) {
  case NILSXP:
    return "NULL";
  case SYMSXP:
    return "symbol";
  case S4SXP:
    return "S4";
  case LGLSXP:
    return "logical vector";
  case INTSXP:
    return "integer vector";
  case REALSXP:
    return "double vector";
  case STRSXP:
    return "character vector";
  case CPLXSXP:
    return "complex vector";
  case RAWSXP:
    return "raw vector";
  case VECSXP:
    return "list";
  case LANGSXP:
    return "quoted call";
  case EXPRSXP:
    return "expression";
  case ENVSXP:
    return "environment";

  case SPECIALSXP:
  case BUILTINSXP:
  case CLOSXP:
    return "function";

  // Everything else can fall back to R's default
  default:
    return std::string(Rf_type2char(TYPEOF(x)));
  }
}

inline SupportedType check_supported_type(SEXP x, const SymbolString& name = String()) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return DPLYR_LGLSXP;
  case INTSXP:
    return DPLYR_INTSXP;
  case REALSXP:
    return DPLYR_REALSXP;
  case CPLXSXP:
    return DPLYR_CPLXSXP;
  case STRSXP:
    return DPLYR_STRSXP;
  case VECSXP:
    return DPLYR_VECSXP;
  default:
    if (name.is_empty()) {
      Rcpp::stop("Unsupported type %s", type_name(x));
    } else {
      Rcpp::stop("Column `%s` must be a vector, not a %s", name.get_utf8_cstring(), type_name(x));
    }
  }
}

inline void check_length(const int actual, const int expected, const char* comment, const SymbolString& name) {
  if (expected == 1) {
    if (actual != expected) {
      stop("incompatible size (%d), expecting one (%s) for column '%s'",
           actual, comment, name.get_utf8_cstring()
          );
    }
  }
  else {
    if (actual != expected && actual != 1) {
      stop("incompatible size (%d), expecting %d (%s) or one for column '%s'",
           actual, expected, comment, name.get_utf8_cstring()
          );
    }
  }
}

}
#endif
