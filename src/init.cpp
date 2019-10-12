#include "pch.h"

#include <tools/utils.h>

#include <dplyr/main.h>
#include <dplyr/symbols.h>

namespace dplyr {

SEXP get_date_classes() {
  static Rcpp::CharacterVector klasses(1, Rf_mkChar("Date"));
  return klasses;
}

inline SEXP init_time_classes() {
  Rcpp::Shield<SEXP> res(Rf_allocVector(STRSXP, 2));
  SET_STRING_ELT(res, 0, Rf_mkChar("POSIXct"));
  SET_STRING_ELT(res, 1, Rf_mkChar("POSIXt"));
  return res;
}

SEXP get_time_classes() {
  static Rcpp::CharacterVector klasses(init_time_classes());
  return klasses;
}

SEXP get_factor_classes() {
  static Rcpp::CharacterVector klasses(1, Rf_mkChar("factor"));
  return klasses;
}

SEXP get_ordered_classes() {
  static Rcpp::CharacterVector klasses(2);
  klasses[0] = "ordered";
  klasses[1] = "factor";
  return klasses;
}

SEXP mark_precious(SEXP x) {
  R_PreserveObject(x);
  return x;
}

SEXP symbols::package = Rf_install("package");
SEXP symbols::n = Rf_install("n");
SEXP symbols::tzone = Rf_install("tzone");
SEXP symbols::units = Rf_install("units");
SEXP symbols::dot_env = Rf_install(".env");
SEXP symbols::dot_data = Rf_install(".data");

SEXP symbols::sum = Rf_install("sum");
SEXP symbols::mean = Rf_install("mean");
SEXP symbols::var = Rf_install("var");
SEXP symbols::sd = Rf_install("sd");
SEXP symbols::n_distinct = Rf_install("n_distinct");
SEXP symbols::first = Rf_install("first");
SEXP symbols::last = Rf_install("last");
SEXP symbols::nth = Rf_install("nth");
SEXP symbols::group_indices = Rf_install("group_indices");
SEXP symbols::min = Rf_install("min");
SEXP symbols::max = Rf_install("max");
SEXP symbols::row_number = Rf_install("row_number");
SEXP symbols::ntile = Rf_install("ntile");
SEXP symbols::min_rank = Rf_install("min_rank");
SEXP symbols::percent_rank = Rf_install("percent_rank");
SEXP symbols::dense_rank = Rf_install("dense_rank");
SEXP symbols::cume_dist = Rf_install("cume_dist");
SEXP symbols::lead = Rf_install("lead");
SEXP symbols::lag = Rf_install("lag");
SEXP symbols::in = Rf_install("%in%");

SEXP symbols::narm = Rf_install("na.rm");
SEXP symbols::default_ = Rf_install("default");

SEXP symbols::dplyr = Rf_install("dplyr");
SEXP symbols::base = Rf_install("base");
SEXP symbols::stats = Rf_install("stats");

SEXP symbols::desc = Rf_install("desc");
SEXP symbols::double_colon = Rf_install("::");
SEXP symbols::na_rm = Rf_install("na.rm");
SEXP symbols::new_env = Rf_install("new.env");
SEXP symbols::comment = Rf_install("comment");
SEXP symbols::groups = Rf_install("groups");
SEXP symbols::vars = Rf_install("vars");
SEXP symbols::position = Rf_install("position");

SEXP symbols::op_minus = Rf_install("-");
SEXP symbols::str = Rf_install("str");
SEXP symbols::dot_Internal = Rf_install(".Internal");
SEXP symbols::inspect = Rf_install("inspect");
SEXP symbols::dot = Rf_install(".");
SEXP symbols::dot_x = Rf_install(".x");
SEXP symbols::drop = Rf_install("drop");

SEXP symbols::rlang = Rf_install("rlang");
SEXP symbols::eval_tidy = Rf_install("eval_tidy");
SEXP symbols::quote = Rf_install("quote");
SEXP symbols::dot_drop = Rf_install(".drop");
SEXP symbols::warn_deprecated = Rf_install("warn_deprecated");
SEXP symbols::signal_soft_deprecated = Rf_install("signal_soft_deprecated");
SEXP symbols::call = Rf_install("call");
SEXP symbols::env = Rf_install("env");
SEXP symbols::fun = Rf_install("fun");
SEXP symbols::cpp_class = Rf_install("cpp_class");
SEXP symbols::levels = Rf_install("levels");
SEXP symbols::labels = Rf_install("labels");
SEXP symbols::indices = Rf_install("indices");
SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::names = R_NamesSymbol;
SEXP symbols::formula = Rf_install("formula");
SEXP fns::quote = Rf_eval(Rf_install("quote"), R_BaseEnv);

SEXP vectors::factor = get_factor_classes();
SEXP vectors::ordered = get_ordered_classes();
SEXP vectors::unbound_sentinel = mark_precious(Rf_allocVector(RAWSXP, 0));

SEXP strings::POSIXct = STRING_ELT(get_time_classes(), 0);
SEXP strings::POSIXt = STRING_ELT(get_time_classes(), 1);
SEXP strings::Date = STRING_ELT(get_date_classes(), 0);
SEXP strings::factor = STRING_ELT(vectors::factor, 0);
SEXP strings::ordered = STRING_ELT(vectors::ordered, 0);

}
