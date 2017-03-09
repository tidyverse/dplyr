#ifndef dplyr_join_match_H
#define dplyr_join_match_H

#include <dplyr/comparisons.h>

namespace dplyr {

  // not defined on purpose
  template <int LHS_RTYPE, int RHS_RTYPE, bool NA_MATCH>
  struct join_match;

  // specialization when LHS_TYPE == RHS_TYPE
  template <int RTYPE, bool NA_MATCH>
  struct join_match<RTYPE, RTYPE, NA_MATCH> {
    typedef comparisons<RTYPE> compare;
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

    static inline bool is_match(STORAGE lhs, STORAGE rhs) {
      return compare::equal_or_both_na(lhs, rhs) && (NA_MATCH || !compare::is_na(lhs));
    }
  };

  // NaN also don't match for reals
  template <bool NA_MATCH>
  struct join_match<REALSXP, REALSXP, NA_MATCH> {
    typedef comparisons<REALSXP> compare;

    static inline bool is_match(double lhs, double rhs) {
      if (NA_MATCH)
        return compare::equal_or_both_na(lhs, rhs);
      else
        return lhs == rhs && (NA_MATCH || (!compare::is_na(lhs) && !compare::is_nan(lhs)));
    }
  };

  // works for both LHS_RTYPE = INTSXP and LHS_RTYPE = LGLSXP
  template <int LHS_RTYPE, bool NA_MATCH>
  struct join_match_int_double {
    static inline bool is_match(int lhs, double rhs) {
      LOG_VERBOSE << lhs << " " << rhs;
      if (double(lhs) == rhs) {
        return (lhs != NA_INTEGER);
      }
      else {
        if (NA_MATCH)
          return (lhs == NA_INTEGER && ISNA(rhs));
        else
          return false;
      }
    }
  };

  template <bool NA_MATCH>
  struct join_match<INTSXP, REALSXP, NA_MATCH> : join_match_int_double<INTSXP, NA_MATCH> {};

  template <bool NA_MATCH>
  struct join_match<LGLSXP, REALSXP, NA_MATCH> : join_match_int_double<LGLSXP, NA_MATCH> {};

  template <int RHS_RTYPE, bool NA_MATCH>
  struct join_match_double_int {
    static inline bool is_match(double lhs, int rhs) {
      return join_match_int_double<RHS_RTYPE, NA_MATCH>::is_match(rhs, lhs);
    }
  };

  template <bool NA_MATCH>
  struct join_match<REALSXP, INTSXP, NA_MATCH> : join_match_double_int<INTSXP, NA_MATCH> {};

  template <bool NA_MATCH>
  struct join_match<REALSXP, LGLSXP, NA_MATCH> : join_match_double_int<LGLSXP, NA_MATCH> {};

  template <bool NA_MATCH>
  struct join_match<INTSXP, LGLSXP, NA_MATCH> : join_match<INTSXP, INTSXP, NA_MATCH> {};

  template <bool NA_MATCH>
  struct join_match<LGLSXP, INTSXP, NA_MATCH> : join_match<INTSXP, INTSXP, NA_MATCH> {};

}

#endif // #ifndef dplyr_join_match_H
