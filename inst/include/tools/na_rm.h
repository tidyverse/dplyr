#ifndef dplyr_tools_na_rm_h
#define dplyr_tools_na_rm_h

namespace dplyr {

  enum NaRmResult {
    WRONG_TAG = 0,
    INVALID = 1,
    NA_RM_FALSE = 2,
    NA_RM_TRUE = 3
  };

  inline NaRmResult eval_na_rm(SEXP x) {
    if (TAG(x) != R_NaRmSymbol)
      return WRONG_TAG;

    SEXP narme = CAR(x);
    SEXP narm = r_constfold(narme);
    if (TYPEOF(narm) != LGLSXP)
      return INVALID;
    if (LENGTH(narm) != 1)
      return INVALID;

    if (LOGICAL(narm)[0] == TRUE)
      return NA_RM_TRUE;

    if (LOGICAL(narm)[0] == FALSE)
      return NA_RM_FALSE;

    return INVALID;
  }

}

#endif // dplyr_tools_na_rm_h
