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
  static SEXP dot_dot_group_size;
  static SEXP dot_dot_group_number;
  static SEXP mask;
  static SEXP caller;

  static SEXP stop_filter_incompatible_size;
  static SEXP stop_filter_incompatible_type;
};

struct vectors {
  static SEXP classes_vctrs_list_of;
  static SEXP classes_tbl_df;
  static SEXP empty_int_vector;
};

void stop_filter_incompatible_size(R_xlen_t i, R_xlen_t group_index, R_xlen_t nres, R_xlen_t n, SEXP data);
void stop_filter_incompatible_type(R_xlen_t i, SEXP column_name, R_xlen_t group_index, SEXP result, SEXP data);

} // namespace dplyr

namespace rlang {
SEXP eval_tidy(SEXP expr, SEXP data, SEXP env);
}

namespace vctrs {
bool vec_is_vector(SEXP x) ;
R_len_t short_vec_size(SEXP x) ;
}

SEXP dplyr_expand_groups(SEXP old_groups, SEXP positions, SEXP s_nr);
SEXP dplyr_filter_update_rows(SEXP s_n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes);
SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right);
SEXP dplyr_cumall(SEXP x);
SEXP dplyr_cumany(SEXP x);
SEXP dplyr_cummean(SEXP x);
SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_nr_df, SEXP s_check_bounds);
SEXP dplyr_group_keys_impl(SEXP data);
SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private, SEXP env_context);
SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i);
SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i);
SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP env_context, SEXP s_n, SEXP full_data, SEXP env_filter);
SEXP dplyr_vec_sizes(SEXP chunks);
SEXP dplyr_validate_summarise_sizes(SEXP size, SEXP chunks);
SEXP dplyr_group_indices(SEXP data, SEXP s_nr);

#endif
