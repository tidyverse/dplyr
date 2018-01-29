#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

#include <tools/SymbolVector.h>

void check_valid_colnames(const DataFrame& df, bool warn_only = false);
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

extern SEXP (*rlang_quo_get_expr)(SEXP quo);
extern SEXP (*rlang_quo_set_expr)(SEXP quo, SEXP expr);
extern SEXP (*rlang_quo_get_env)(SEXP quo);
extern SEXP (*rlang_quo_set_env)(SEXP quo, SEXP env);
extern SEXP (*rlang_new_quosure)(SEXP expr, SEXP env);
extern SEXP (*rlang_is_quosure)(SEXP x);
extern SEXP (*rlang_as_data_pronoun)(SEXP data);
extern SEXP (*rlang_as_data_mask)(SEXP data, SEXP parent);
extern SEXP (*rlang_new_data_mask)(SEXP bottom, SEXP top, SEXP parent);

} // internal


} // dplyr

#endif // #ifndef dplyr_tools_utils_H
