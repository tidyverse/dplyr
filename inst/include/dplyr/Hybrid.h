#ifndef dplyr_dplyr_Hybrid_H
#define dplyr_dplyr_Hybrid_H

namespace dplyr {
  class LazySubsets;
  class Result;

  Result* get_handler(SEXP, const LazySubsets&, const Environment&);
  Result* nth_prototype(SEXP call, const LazySubsets& subsets, int nargs);
  Result* first_prototype(SEXP call, const LazySubsets& subsets, int nargs);
  Result* last_prototype(SEXP call, const LazySubsets& subsets, int nargs);

}

bool argmatch(const std::string& target, const std::string& s);
bool can_simplify(SEXP);

#endif // dplyr_dplyr_Hybrid_H
