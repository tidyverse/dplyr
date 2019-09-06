#include "pch.h"
#include <dplyr/main.h>

#include <tools/utils.h>
#include <dplyr/allow_list.h>
#include <tools/collapse.h>
#include <tools/bad.h>
#include <dplyr/symbols.h>
#include <dplyr/lifecycle.h>

// [[Rcpp::export(rng = false)]]
void check_valid_names(const Rcpp::CharacterVector& names, bool warn_only = false) {
  R_xlen_t n = XLENGTH(names);

  std::vector<int> which_na;
  which_na.reserve(n);

  for (int i = 0; i < n; ++i) {
    if (STRING_ELT(names, i) == R_NaString) {
      which_na.push_back(i + 1);
    }
  }

  if (which_na.size() > 0) {
    dplyr::SymbolVector which_na_symbols(Rcpp::wrap(which_na));
    Rcpp::String msg = msg_bad_cols(which_na_symbols, "cannot have NA as name");
    if (warn_only)
      Rcpp::warning(msg.get_cstring());
    else
      Rcpp::stop(msg.get_cstring());
  }

  Rcpp::LogicalVector dup(duplicated(names));
  if (any(dup).is_true()) {
    Rcpp::String msg = msg_bad_cols(dplyr::SymbolVector(static_cast<SEXP>(names[dup])), "must have a unique name");
    if (warn_only)
      Rcpp::warning(msg.get_cstring());
    else
      Rcpp::stop(msg.get_cstring());
  }
}

SEXP shared_SEXP(SEXP x) {
  MARK_NOT_MUTABLE(x);
  return x;
}

namespace dplyr {

Rcpp::CharacterVector default_chars(SEXP x, R_xlen_t len) {
  if (Rf_isNull(x)) return Rcpp::CharacterVector(len);
  return x;
}

void copy_attrib(SEXP out, SEXP origin, SEXP symbol) {
  Rf_setAttrib(out, symbol, Rcpp::Shield<SEXP>(Rf_getAttrib(origin, symbol)));
}

SEXP set_class(SEXP x, const Rcpp::CharacterVector& class_) {
  SEXP class_attr = class_.length() == 0 ? R_NilValue : (SEXP)class_;
  return Rf_setAttrib(x, R_ClassSymbol, class_attr);
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

SEXP vec_names_or_empty(SEXP x) {
  SEXP nms = Rf_getAttrib(x, R_NamesSymbol);
  if (Rf_isNull(nms)) {
    return Rf_allocVector(STRSXP, LENGTH(x));
  }
  return nms;
}

bool is_str_empty(SEXP str) {
  const char* c_str = CHAR(str);
  return strcmp(c_str, "") == 0;
}

bool has_name_at(SEXP x, R_len_t i) {
  SEXP nms = vec_names(x);
  return TYPEOF(nms) == STRSXP && !is_str_empty(STRING_ELT(nms, i));
}

// [[Rcpp::export(rng = false)]]
bool is_data_pronoun(SEXP expr) {
  if (TYPEOF(expr) != LANGSXP || Rf_length(expr) != 3)
    return false;

  SEXP first = CADR(expr);
  if (first != dplyr::symbols::dot_data)
    return false;

  SEXP second = CADDR(expr);
  SEXP fun = CAR(expr);

  // .data$x or .data$"x"
  if (fun == R_DollarSymbol && (TYPEOF(second) == SYMSXP || TYPEOF(second) == STRSXP))
    return true;

  // .data[["x"]]
  if (fun == R_Bracket2Symbol && TYPEOF(second) == STRSXP)
    return true;

  return false;
}

// [[Rcpp::export(rng = false)]]
bool is_variable_reference(SEXP expr) {
  // x
  if (TYPEOF(expr) == SYMSXP)
    return true;

  return is_data_pronoun(expr);
}

// [[Rcpp::export(rng = false)]]
bool quo_is_variable_reference(SEXP quo) {
  return is_variable_reference(CADR(quo));
}

// [[Rcpp::export(rng = false)]]
bool quo_is_data_pronoun(SEXP quo) {
  return is_data_pronoun(CADR(quo));
}

int get_size(SEXP x) {
  if (Rf_isMatrix(x)) {
    return INTEGER(Rf_getAttrib(x, R_DimSymbol))[0];
  } else if (Rf_inherits(x, "data.frame")) {
    return Rcpp::DataFrame(x).nrows();
  } else {
    return Rf_length(x);
  }
}

namespace dplyr {
namespace lifecycle {

void warn_deprecated(const std::string& s) {
  static Rcpp::Environment ns_dplyr(Rcpp::Environment::namespace_env("dplyr"));

  Rcpp::CharacterVector msg(Rcpp::CharacterVector::create(s));
  Rcpp::Shield<SEXP> call(Rf_lang2(symbols::warn_deprecated, msg));

  Rcpp::Rcpp_eval(call, ns_dplyr);
}

void signal_soft_deprecated(const std::string& s, SEXP caller_env) {
  static Rcpp::Environment ns_dplyr(Rcpp::Environment::namespace_env("dplyr"));

  Rcpp::CharacterVector msg(Rcpp::CharacterVector::create(s));
  Rcpp::Shield<SEXP> call(Rf_lang4(symbols::signal_soft_deprecated, msg, msg, caller_env));

  Rcpp::Rcpp_eval(call, ns_dplyr);
}


}
}


