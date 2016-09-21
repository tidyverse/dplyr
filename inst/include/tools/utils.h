#ifndef dplyr_tools_utils_H
#define dplyr_tools_utils_H

void assert_all_white_list(const DataFrame&);
SEXP shared_SEXP(SEXP x);
SEXP shallow_copy(const List& data);
SEXP pairlist_shallow_copy(SEXP p);
void copy_attributes(SEXP out, SEXP data);
void copy_most_attributes(SEXP out, SEXP data);

#endif // #ifndef dplyr_tools_utils_H
