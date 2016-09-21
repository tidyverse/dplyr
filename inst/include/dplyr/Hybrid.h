#ifndef dplyr_dplyr_hybrid_H
#define dplyr_dplyr_hybrid_H

dplyr::Result* get_handler(SEXP, const dplyr::LazySubsets&, const Environment&);
dplyr::Result* nth_prototype(SEXP call, const dplyr::LazySubsets& subsets, int nargs);
dplyr::Result* first_prototype(SEXP call, const dplyr::LazySubsets& subsets, int nargs);
dplyr::Result* last_prototype(SEXP call, const dplyr::LazySubsets& subsets, int nargs);

bool argmatch(const std::string& target, const std::string& s);

bool can_simplify(SEXP);

#endif // dplyr_dplyr_hybrid_H
