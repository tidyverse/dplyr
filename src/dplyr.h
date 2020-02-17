#ifndef DPLYR_DPLYR_H
#define DPLYR_DPLYR_H

#define R_NOREMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

namespace dplyr {

struct envs {
  static SEXP ns_dplyr;
};

struct symbols {
  static SEXP groups;
  static SEXP levels;
  static SEXP ptype;
  static SEXP vars;
  static SEXP current_group;
  static SEXP current_expression;
  static SEXP rows;
  static SEXP mask;
  static SEXP caller;
  static SEXP resolved;
  static SEXP bindings;
  static SEXP which_used;
  static SEXP dot_drop;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP classes_tbl_df;
  static SEXP empty_int_vector;
};

void stop_filter_incompatible_size(R_xlen_t i, R_xlen_t nres, R_xlen_t n);
void stop_filter_incompatible_type(R_xlen_t i, SEXP column_name, SEXP result);
void stop_summarise_unsupported_type(SEXP result);
void stop_summarise_incompatible_size(int size, R_xlen_t index_group);

} // namespace dplyr

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
}

namespace vctrs {
bool vec_is_vector(SEXP x) ;
R_len_t short_vec_size(SEXP x) ;
SEXP vec_assign_impl(SEXP, SEXP, SEXP, bool);
SEXP vec_cast(SEXP, SEXP, SEXP, SEXP);
SEXP short_vec_init(SEXP, R_len_t);
}

SEXP dplyr_expand_groups(SEXP old_groups, SEXP positions, SEXP s_nr);
SEXP dplyr_filter_update_rows(SEXP s_n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes);
SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right);
SEXP dplyr_cumall(SEXP x);
SEXP dplyr_cumany(SEXP x);
SEXP dplyr_cummean(SEXP x);
SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_check_bounds);
SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private);
SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private);
SEXP dplyr_vec_unchop(SEXP chunks, SEXP rows, SEXP nrows, SEXP ptype);
SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter);
SEXP dplyr_vec_sizes(SEXP chunks);
SEXP dplyr_validate_summarise_sizes(SEXP size, SEXP chunks);
SEXP dplyr_group_indices(SEXP data, SEXP s_nr);
SEXP dplyr_group_keys(SEXP group_data);

#define DPLYR_MASK_INIT()                                                  \
SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows)); \
R_xlen_t ngroups = XLENGTH(rows);                                          \
SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask)); \
SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller)); \
SEXP bindings = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::bindings)); \
SEXP current_group = PROTECT(Rf_ScalarInteger(NA_INTEGER));                    \
Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);       \
int* p_current_group = INTEGER(current_group)

#define DPLYR_MASK_FINALISE() UNPROTECT(5);

#define DPLYR_MASK_SET_GROUP(INDEX)                                                  \
*p_current_group = INDEX + 1;   \
SEXP resolved = Rf_findVarInFrame(env_private, dplyr::symbols::resolved);            \
SEXP which_used = Rf_findVarInFrame(env_private, dplyr::symbols::which_used); \
int* p_which_used = INTEGER(which_used);                       \
SEXP names_resolved = Rf_getAttrib(resolved, R_NamesSymbol);       \
R_xlen_t n_resolved = XLENGTH(which_used);                           \
for (R_xlen_t i_resolved = 0; i_resolved < n_resolved; i_resolved++) { \
  int idx_promise = p_which_used[i_resolved] - 1;              \
  Rf_defineVar( \
    Rf_installChar(STRING_ELT(names_resolved, idx_promise)), \
    VECTOR_ELT(VECTOR_ELT(resolved, idx_promise), i), bindings \
  ); \
}


#define DPLYR_MASK_EVAL(quo) rlang::eval_tidy(quo, mask, caller)

#endif
