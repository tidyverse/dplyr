#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/registration.h>
#include <dplyr/symbols.h>

using namespace Rcpp;

SEXP get_cache() {
  static SEXP cache = 0;
  if (!cache) {
    SEXP vec = PROTECT(Rf_allocVector(VECSXP, 2));
    SEXP date_classes = PROTECT(Rf_mkString("Date"));
    SET_VECTOR_ELT(vec, 0, date_classes);
    CharacterVector time_classes = CharacterVector::create("POSIXct", "POSIXt");
    SET_VECTOR_ELT(vec, 1, time_classes);
    UNPROTECT(2);
    R_PreserveObject(vec);
    cache = vec;
  }
  return cache;
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
SEXP get_date_classes() {
  return VECTOR_ELT(get_cache(), 0);
}

// [[Rcpp::interfaces(cpp)]]
// [[Rcpp::export]]
SEXP get_time_classes() {
  return VECTOR_ELT(get_cache(), 1);
}

namespace dplyr {

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

SEXP symbols::op_minus = Rf_install("-");
SEXP symbols::str = Rf_install("str");
SEXP symbols::dot_Internal = Rf_install(".Internal");
SEXP symbols::inspect = Rf_install("inspect");
SEXP symbols::dot_top_env = Rf_install(".top_env");
}
