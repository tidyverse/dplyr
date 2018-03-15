#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

#include <tools/SymbolVector.h>

void check_valid_colnames(const DataFrame& df, bool warn_only = false);
void check_range_one_based(int x, int max);
void assert_all_white_list(const DataFrame&);
SEXP shared_SEXP(SEXP x);
SEXP shallow_copy(const List& data);
SEXP pairlist_shallow_copy(SEXP p);
void copy_attributes(SEXP out, SEXP data);
void strip_index(DataFrame x);
SEXP null_if_empty(SEXP x);

bool is_vector(SEXP x);
bool is_atomic(SEXP x);

SEXP vec_names(SEXP x);
SEXP vec_names_or_empty(SEXP x);
bool is_str_empty(SEXP str);
bool has_name_at(SEXP x, R_len_t i);
SEXP name_at(SEXP x, size_t i);

SEXP f_env(SEXP x);
bool is_quosure(SEXP x);
SEXP maybe_rhs(SEXP x);


namespace dplyr {

std::string get_single_class(SEXP x);
CharacterVector default_chars(SEXP x, R_xlen_t len);
CharacterVector get_class(SEXP x);
SEXP set_class(SEXP x, const CharacterVector& class_);
CharacterVector get_levels(SEXP x);
SEXP set_levels(SEXP x, const CharacterVector& levels);
bool same_levels(SEXP left, SEXP right);
bool character_vector_equal(const CharacterVector& x, const CharacterVector& y);

SymbolVector get_vars(SEXP x, bool duplicate = false);
void set_vars(SEXP x, const SymbolVector& vars);
void copy_vars(SEXP target, SEXP source);

// effectively the same as copy_attributes but without names and dims
inline void copy_most_attributes(SEXP out, SEXP data) {
  Rf_copyMostAttrib(data, out);
}


namespace internal {

// *INDENT-OFF*
struct rlang_api_ptrs_t {
  SEXP (*quo_get_expr)(SEXP quo);
  SEXP (*quo_set_expr)(SEXP quo, SEXP expr);
  SEXP (*quo_get_env)(SEXP quo);
  SEXP (*quo_set_env)(SEXP quo, SEXP env);
  SEXP (*new_quosure)(SEXP expr, SEXP env);
  SEXP (*is_quosure)(SEXP x);
  SEXP (*as_data_pronoun)(SEXP data);
  SEXP (*as_data_mask)(SEXP data, SEXP parent);
  SEXP (*new_data_mask)(SEXP bottom, SEXP top, SEXP parent);

  rlang_api_ptrs_t() {
    quo_get_expr =     (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_expr");
    quo_set_expr =     (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_expr");
    quo_get_env =      (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_quo_get_env");
    quo_set_env =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_quo_set_env");
    new_quosure =      (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_new_quosure");
    is_quosure =       (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_is_quosure");
    as_data_pronoun =  (SEXP (*)(SEXP))             R_GetCCallable("rlang", "rlang_as_data_pronoun");
    as_data_mask =     (SEXP (*)(SEXP, SEXP))       R_GetCCallable("rlang", "rlang_as_data_mask");
    new_data_mask =    (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_new_data_mask");
  }
};
// *INDENT-ON*

const rlang_api_ptrs_t& rlang_api();

} // namespace internal


} // dplyr

#endif // #ifndef dplyr_tools_utils_H
