#include "dplyr.h"

namespace dplyr {
void stop_filter_incompatible_size(R_xlen_t i, SEXP quos, R_xlen_t nres, R_xlen_t n) {
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));

  SEXP s_expected_size = PROTECT(Rf_ScalarInteger(n));
  SEXP s_size = PROTECT(Rf_ScalarInteger(nres));
  SEXP sym_stop_filter_incompatible_size = Rf_install("stop_filter_incompatible_size");

  SEXP call = Rf_lang5(sym_stop_filter_incompatible_size,
    s_index_expression, quos, s_size, s_expected_size
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_filter_incompatible_type(R_xlen_t i, SEXP quos, SEXP column_name, SEXP result){
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP sym_stop_filter_incompatible_type = Rf_install("stop_filter_incompatible_type");

  SEXP call = Rf_lang5(sym_stop_filter_incompatible_type,
    s_index_expression, quos, column_name, result
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_summarise_unsupported_type(SEXP result) {
  SEXP sym_stop_summarise_unsupported_type = Rf_install("stop_summarise_unsupported_type");
  SEXP call = Rf_lang2(sym_stop_summarise_unsupported_type, result);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_summarise_incompatible_size(int index_group, int index_expression, int expected_size, int size) {
  SEXP s_size = PROTECT(Rf_ScalarInteger(size));
  SEXP s_expected_size = PROTECT(Rf_ScalarInteger(expected_size));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(index_group + 1));
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(index_expression + 1));
  SEXP sym_stop_summarise_incompatible_size = Rf_install("stop_summarise_incompatible_size");
  SEXP call = Rf_lang5(sym_stop_summarise_incompatible_size, s_index_group, s_index_expression, s_expected_size, s_size);
  Rf_eval(call, dplyr::envs::ns_dplyr);
}
}

