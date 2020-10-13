#ifndef DPLYR_DPLYR_H
#define DPLYR_DPLYR_H

#define R_NOREMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define UTF8_MASK (1<<3)
#define ASCII_MASK (1<<6)

#define IS_ASCII(x) (LEVELS(x) & ASCII_MASK)
#define IS_UTF8(x) (LEVELS(x) & UTF8_MASK)

namespace dplyr {

struct envs {
  static SEXP ns_dplyr;
  static SEXP ns_vctrs;
};

struct symbols {
  static SEXP groups;
  static SEXP levels;
  static SEXP ptype;
  static SEXP current_group;
  static SEXP current_expression;
  static SEXP rows;
  static SEXP mask;
  static SEXP caller;
  static SEXP resolved;
  static SEXP bindings;
  static SEXP dot_drop;
  static SEXP abort_glue;
  static SEXP dot_indices;
  static SEXP colon;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP empty_int_vector;

  static SEXP names_expanded;
  static SEXP names_summarise_recycle_chunks;
};

struct functions {
  static SEXP vec_chop;
  static SEXP dot_subset2;
  static SEXP list;
};

} // namespace dplyr

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
}

namespace vctrs {
bool vec_is_vector(SEXP x) ;
R_len_t short_vec_size(SEXP x) ;
SEXP short_vec_recycle(SEXP x, R_len_t n);

inline bool vec_is_list(SEXP x) {
  SEXP call = PROTECT(Rf_lang2(Rf_install("vec_is_list"), x));
  SEXP res = Rf_eval(call, dplyr::envs::ns_vctrs);
  UNPROTECT(1);
  return LOGICAL(res)[0];
}

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
SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP s_n, SEXP env_filter);
SEXP dplyr_summarise_recycle_chunks(SEXP chunks, SEXP rows, SEXP ptypes);
SEXP dplyr_group_indices(SEXP data, SEXP s_nr);
SEXP dplyr_group_keys(SEXP group_data);

SEXP dplyr_mask_set(SEXP env_private, SEXP s_name, SEXP chunks);
SEXP dplyr_mask_add(SEXP env_private, SEXP s_name, SEXP chunks);

SEXP dplyr_lazy_vec_chop(SEXP data, SEXP rows);
SEXP dplyr_data_masks_setup(SEXP chops, SEXP data, SEXP rows);
SEXP env_resolved(SEXP env, SEXP names);

#define DPLYR_MASK_INIT()                                                                    \
SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));                   \
R_xlen_t ngroups = XLENGTH(rows);                                                            \
SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));               \
SEXP masks = PROTECT(Rf_findVarInFrame(env_private, Rf_install("masks")));                   \
SEXP current_group = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::current_group)); \
int* p_current_group = INTEGER(current_group)

#define DPLYR_MASK_FINALISE() UNPROTECT(4);

#define DPLYR_MASK_SET_GROUP(INDEX) *p_current_group = INDEX + 1;

#define DPLYR_MASK_EVAL(quo, GROUP) rlang::eval_tidy(quo, VECTOR_ELT(masks, i), caller)

#define DPLYR_ERROR_INIT(n)                                    \
  SEXP error_data = PROTECT(Rf_allocVector(VECSXP, n));              \
  SEXP error_names = PROTECT(Rf_allocVector(STRSXP, n));             \
  Rf_setAttrib(error_data, R_NamesSymbol, error_names);

#define DPLYR_ERROR_MESG_INIT(n)                               \
  SEXP error_message = PROTECT(Rf_allocVector(STRSXP, n));     \

#define DPLYR_ERROR_SET(i, name, value)                        \
  SET_VECTOR_ELT(error_data, i, value);                        \
  SET_STRING_ELT(error_names, i, Rf_mkChar(name));

#define DPLYR_ERROR_MSG_SET(i, msg)                        \
  SET_STRING_ELT(error_message, i, Rf_mkChar(msg));                          \

#define DPLYR_ERROR_THROW(klass)                                    \
  SEXP error_class = PROTECT(Rf_mkString(klass));              \
  SEXP error_call = PROTECT(Rf_lang4(dplyr::symbols::abort_glue, error_message, error_data, error_class)); \
  Rf_eval(error_call, dplyr::envs::ns_dplyr);                  \
  UNPROTECT(5) ; // for rchk

#endif
