#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

#include <tools/SymbolVector.h>
#include <tools/collapse.h>

void assert_all_white_list(const DataFrame&);
SEXP shared_SEXP(SEXP x);
SEXP shallow_copy(const List& data);
SEXP pairlist_shallow_copy(SEXP p);
void copy_attributes(SEXP out, SEXP data);
void strip_index(DataFrame x);

bool is_vector(SEXP x);
bool is_atomic(SEXP x);

SEXP vec_names(SEXP x);
bool is_str_empty(SEXP str);
bool has_name_at(SEXP x, R_len_t i);
SEXP name_at(SEXP x, size_t i);

namespace dplyr {

inline bool character_vector_equal(const CharacterVector& x, const CharacterVector& y) {
  if ((SEXP)x == (SEXP)y) return true;

  if (x.length() != y.length())
    return false;

  for (R_xlen_t i = 0; i < x.length(); ++i) {
    SEXP xi = x[i];
    SEXP yi = y[i];

    // Ideally we'd use Rf_Seql(), but this is not exported.
    if (Rf_NonNullStringMatch(xi, yi)) continue;
    if (xi == NA_STRING && yi == NA_STRING) continue;
    if (xi == NA_STRING || yi == NA_STRING)
      return false;
    if (CHAR(xi)[0] == 0 && CHAR(yi)[0] == 0) continue;
    return false;
  }

  return true;
}


inline std::string get_single_class(SEXP x) {
  SEXP klass = Rf_getAttrib(x, R_ClassSymbol);
  if (!Rf_isNull(klass)) {
    CharacterVector classes(klass);
    return collapse_utf8(classes, "/");
  }

  if (Rf_isMatrix(x)) {
    return "matrix";
  }

  switch (TYPEOF(x)) {
  case INTSXP:
    return "integer";
  case REALSXP :
    return "numeric";
  case LGLSXP:
    return "logical";
  case STRSXP:
    return "character";

  case VECSXP:
    return "list";
  default:
    break;
  }

  // just call R to deal with other cases
  // we could call R_data_class directly but we might get a "this is not part of the api"
  klass = Rf_eval(Rf_lang2(Rf_install("class"), x), R_GlobalEnv);
  return CHAR(STRING_ELT(klass, 0));
}


// effectively the same as copy_attributes but without names and dims
inline void copy_most_attributes(SEXP out, SEXP data) {
  Rf_copyMostAttrib(data, out);
}

inline CharacterVector default_chars(SEXP x, R_xlen_t len) {
  if (Rf_isNull(x)) return CharacterVector(len);
  return x;
}

inline CharacterVector get_class(SEXP x) {
  SEXP class_attr = Rf_getAttrib(x, R_ClassSymbol);
  return default_chars(class_attr, 0);
}

inline SEXP set_class(SEXP x, const CharacterVector& class_) {
  SEXP class_attr = class_.length() == 0 ? R_NilValue : (SEXP)class_;
  return Rf_setAttrib(x, R_ClassSymbol, class_attr);
}

inline SymbolVector get_vars(SEXP x) {
  static SEXP vars_symbol = Rf_install("vars");
  return SymbolVector(Rf_getAttrib(x, vars_symbol));
}

inline void set_vars(SEXP x, const SymbolVector& vars) {
  static SEXP vars_symbol = Rf_install("vars");
  Rf_setAttrib(x, vars_symbol, vars.get_vector());
}

inline void copy_vars(SEXP target, SEXP source) {
  set_vars(target, get_vars(source));
}

inline CharacterVector get_levels(SEXP x) {
  SEXP levels_attr = Rf_getAttrib(x, R_LevelsSymbol);
  return default_chars(levels_attr, 0);
}

inline SEXP set_levels(SEXP x, const CharacterVector& levels) {
  return Rf_setAttrib(x, R_LevelsSymbol, levels);
}

inline bool same_levels(SEXP left, SEXP right) {
  return character_vector_equal(get_levels(left), get_levels(right));
}


}

#endif // #ifndef dplyr_tools_utils_H
