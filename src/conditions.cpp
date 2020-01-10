#include "dplyr.h"

namespace dplyr {
void stop_filter_incompatible_size(R_xlen_t i, R_xlen_t group_index, R_xlen_t nres, R_xlen_t n, SEXP data) {
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(group_index + 1));

  SEXP s_expected_size = PROTECT(Rf_ScalarInteger(n));
  SEXP s_size = PROTECT(Rf_ScalarInteger(nres));
  SEXP sym_stop_filter_incompatible_size = Rf_install("stop_filter_incompatible_size");

  SEXP call = Rf_lang6(
    sym_stop_filter_incompatible_size,
    s_index_expression, s_index_group, s_size, s_expected_size, data
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_filter_incompatible_type(R_xlen_t i, SEXP column_name, R_xlen_t group_index, SEXP result, SEXP data){
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(group_index + 1));
  SEXP sym_stop_filter_incompatible_type = Rf_install("stop_filter_incompatible_type");

  SEXP call = Rf_lang6(
    sym_stop_filter_incompatible_type,
    s_index_expression, column_name, s_index_group, result, data
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_summarise_unsupported_type(SEXP result) {
  SEXP sym_stop_summarise_unsupported_type = Rf_install("stop_summarise_unsupported_type");
  SEXP call = Rf_lang2(sym_stop_summarise_unsupported_type, result);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_summarise_incompatible_size(int size) {
  SEXP s_size = PROTECT(Rf_ScalarInteger(size));
  SEXP sym_stop_incompatible_size = Rf_install("stop_incompatible_size");
  SEXP call = Rf_lang2(sym_stop_incompatible_size, s_size);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}
}

