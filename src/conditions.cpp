#include "dplyr.h"

namespace dplyr {
void stop_filter_incompatible_size(R_xlen_t i, R_xlen_t group_index, R_xlen_t nres, R_xlen_t n, SEXP data) {
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(group_index + 1));

  SEXP s_expected_size = PROTECT(Rf_ScalarInteger(n));
  SEXP s_size = PROTECT(Rf_ScalarInteger(nres));

  SEXP call = Rf_lang6(
    dplyr::symbols::stop_filter_incompatible_size,
    s_index_expression, s_index_group, s_size, s_expected_size, data
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

void stop_filter_incompatible_type(R_xlen_t i, R_xlen_t column_index, R_xlen_t group_index, SEXP result, SEXP data) {
  SEXP s_index_expression = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP s_column_index = PROTECT(Rf_ScalarInteger(i + 1));
  SEXP s_index_group = PROTECT(Rf_ScalarInteger(group_index + 1));

  SEXP call = Rf_lang6(
    dplyr::symbols::stop_filter_incompatible_type,
    s_index_expression, s_column_index, s_index_group, result, data
  );
  Rf_eval(call, dplyr::envs::ns_dplyr);
}

}
