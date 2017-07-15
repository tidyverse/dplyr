#include "pch.h"
#include <dplyr/main.h>

#include <tools/utils.h>
#include <dplyr/white_list.h>
#include <tools/collapse.h>
#include <dplyr/bad.h>

using namespace Rcpp;

// [[Rcpp::export]]
void assert_all_white_list(const DataFrame& data) {
  // checking variables are on the white list
  int nc = data.size();
  for (int i = 0; i < nc; i++) {
    if (!white_list(data[i])) {
      SymbolVector names = data.names();
      const SymbolString& name_i = names[i];
      SEXP v = data[i];

      SEXP klass = Rf_getAttrib(v, R_ClassSymbol);
      if (!Rf_isNull(klass)) {
        bad_col(name_i, "is of unsupported class {type}",
                _["type"] = get_single_class(v));
      }
      else {
        bad_col(name_i, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(v)));
      }
    }
  }
}

SEXP shared_SEXP(SEXP x) {
  SET_NAMED(x, 2);
  return x;
}

// [[Rcpp::export]]
SEXP shallow_copy(const List& data) {
  int n = data.size();
  List out(n);
  for (int i = 0; i < n; i++) {
    out[i] = shared_SEXP(data[i]);
  }
  copy_attributes(out, data);
  return out;
}

SEXP pairlist_shallow_copy(SEXP p) {
  Shield<SEXP> attr(Rf_cons(CAR(p), R_NilValue));
  SEXP q = attr;
  SET_TAG(q, TAG(p));
  p = CDR(p);
  while (!Rf_isNull(p)) {
    Shield<SEXP> s(Rf_cons(CAR(p), R_NilValue));
    SETCDR(q, s);
    q = CDR(q);
    SET_TAG(q, TAG(p));
    p = CDR(p);
  }
  return attr;
}

void copy_only_attributes(SEXP out, SEXP data) {
  SEXP att = ATTRIB(data);
  const bool has_attributes = !Rf_isNull(att);
  if (has_attributes) {
    LOG_VERBOSE << "copying attributes: " << CharacterVector(List(att).names());

    SET_ATTRIB(out, pairlist_shallow_copy(ATTRIB(data)));
  }
}

void copy_attributes(SEXP out, SEXP data) {
  copy_only_attributes(out, data);
  SET_OBJECT(out, OBJECT(data));
  if (IS_S4_OBJECT(data)) SET_S4_OBJECT(out);
}

SEXP null_if_empty(SEXP x) {
  if (Rf_length(x))
    return x;
  else
    return R_NilValue;
}


namespace dplyr {

std::string get_single_class(SEXP x) {
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

CharacterVector default_chars(SEXP x, R_xlen_t len) {
  if (Rf_isNull(x)) return CharacterVector(len);
  return x;
}

CharacterVector get_class(SEXP x) {
  SEXP class_attr = Rf_getAttrib(x, R_ClassSymbol);
  return default_chars(class_attr, 0);
}

SEXP set_class(SEXP x, const CharacterVector& class_) {
  SEXP class_attr = class_.length() == 0 ? R_NilValue : (SEXP)class_;
  return Rf_setAttrib(x, R_ClassSymbol, class_attr);
}

CharacterVector get_levels(SEXP x) {
  SEXP levels_attr = Rf_getAttrib(x, R_LevelsSymbol);
  return default_chars(levels_attr, 0);
}

SEXP set_levels(SEXP x, const CharacterVector& levels) {
  return Rf_setAttrib(x, R_LevelsSymbol, levels);
}

bool same_levels(SEXP left, SEXP right) {
  return character_vector_equal(get_levels(left), get_levels(right));
}

SEXP list_as_chr(SEXP x) {
  int n = Rf_length(x);
  CharacterVector chr(n);

  for (int i = 0; i != n; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    switch (TYPEOF(elt)) {
    case STRSXP:
      if (Rf_length(chr) == 1) {
        chr[i] = elt;
        continue;
      }
      break;
    case SYMSXP:
      chr[i] = PRINTNAME(elt);
      continue;
    default:
      break;
    }

    stop("The tibble's `vars` attribute has unexpected contents");
  }

  return chr;
}

SymbolVector get_vars(SEXP x) {
  static SEXP vars_symbol = Rf_install("vars");
  RObject vars = Rf_getAttrib(x, vars_symbol);

  switch (TYPEOF(vars)) {
  case NILSXP:
  case STRSXP:
    break;
  case VECSXP:
    vars = list_as_chr(vars);
    break;
  default:
    stop("The tibble's `vars` attribute has unexpected type");
  }

  return SymbolVector(vars);
}

void set_vars(SEXP x, const SymbolVector& vars) {
  static SEXP vars_symbol = Rf_install("vars");
  Rf_setAttrib(x, vars_symbol, null_if_empty(vars.get_vector()));
}

void copy_vars(SEXP target, SEXP source) {
  set_vars(target, get_vars(source));
}

bool character_vector_equal(const CharacterVector& x, const CharacterVector& y) {
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

}

bool is_vector(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
  case VECSXP:
    return true;
  default:
    return false;
  }
}
bool is_atomic(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
  case INTSXP:
  case REALSXP:
  case CPLXSXP:
  case STRSXP:
  case RAWSXP:
    return true;
  default:
    return false;
  }
}

SEXP vec_names(SEXP x) {
  return Rf_getAttrib(x, R_NamesSymbol);
}
bool is_str_empty(SEXP str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}
bool has_name_at(SEXP x, R_len_t i) {
  SEXP nms = vec_names(x);
  return TYPEOF(nms) == STRSXP && !is_str_empty(STRING_ELT(nms, i));
}
SEXP name_at(SEXP x, size_t i) {
  SEXP names = vec_names(x);
  if (Rf_isNull(names))
    return Rf_mkChar("");
  else
    return STRING_ELT(names, i);
}

SEXP f_env(SEXP x) {
  return Rf_getAttrib(x, Rf_install(".Environment"));
}
bool is_quosure(SEXP x) {
  return TYPEOF(x) == LANGSXP
    && Rf_length(x) == 2
    && Rf_inherits(x, "quosure")
    && TYPEOF(f_env(x)) == ENVSXP;
}
SEXP maybe_rhs(SEXP x) {
  if (is_quosure(x))
    return CADR(x);
  else
    return x;
}
