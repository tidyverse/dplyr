#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

void assert_all_white_list(const DataFrame&);
SEXP shared_SEXP(SEXP x);
SEXP shallow_copy(const List& data);
SEXP pairlist_shallow_copy(SEXP p);
void copy_attributes(SEXP out, SEXP data);
void strip_index(DataFrame x);
std::string get_single_class(SEXP x);
CharacterVector get_class(SEXP x);
SEXP set_class(SEXP x, const CharacterVector& class_);
CharacterVector get_levels(SEXP x);
SEXP set_levels(SEXP x, const CharacterVector& levels);
bool character_vector_equal(const CharacterVector& x, const CharacterVector& y);
bool same_levels(SEXP left, SEXP right);

// effectively the same as copy_attributes but without names and dims
inline void copy_most_attributes(SEXP out, SEXP data) {
  Rf_copyMostAttrib(data, out);
}

#endif // #ifndef dplyr_tools_utils_H
