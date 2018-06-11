#ifndef dplyr_DataMask_bindings_promises_H
#define dplyr_DataMask_bindings_promises_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/LazySplitSubsets.h>
#include <dplyr/promise.h>

namespace dplyr {

// in the general case (for grouped and rowwise), the bindings
// environment contains promises of the subsets
template <typename Data>
class DataMask_bindings_promises {
public:
  typedef typename Data::slicing_index Index ;

  DataMask_bindings_promises(SEXP parent_env, LazySplitSubsets& subsets_) :
    mask_bindings(child_env(parent_env)),
    subsets(subsets_),
    n_subsets(subsets.size()),
    promises(),
    forced(n_subsets, false),
    names(subsets.get_names())
  {}

  inline operator SEXP() {
    return mask_bindings;
  }

  void update(const Index& indices) {
    // update promises in mask_promises
    if (indices.group() == 0) {
      set_promises(indices);
    } else {
      update_promises(indices);
    }
  }

private:
  Environment mask_bindings;
  LazySplitSubsets& subsets ;
  int n_subsets;
  std::vector<promise> promises;
  std::vector<bool> forced;
  CharacterVector names;

  inline SEXP get_subset_expr(int i, const Index& indices) {
    static SEXP symb_bracket = Rcpp::traits::same_type<Data, RowwiseDataFrame>::value ? R_Bracket2Symbol : R_BracketSymbol ;
    if (subsets.is_summary(i)) {
      return Rf_lang3(symb_bracket, subsets.get_variable(i), Rf_ScalarInteger(indices.group() + 1));
    } else {
      return Rf_lang3(symb_bracket, subsets.get_variable(i), indices);
    }
  }

  void set_promises(const Index& indices) {
    promises.reserve(n_subsets);
    for (int i = 0; i < n_subsets; i++) {
      promises.push_back(promise(names[i], mask_bindings, get_subset_expr(i, indices)));
    }
  }

  void update_promises(const Index& indices) {
    for (int i = 0; i < n_subsets; i++) {

      promise& p = promises[i];

      if (p.was_forced()) {
        forced[i] = true ;
      } else {
        // otherwise just need to update the expression
        update_promise_index(p, indices);
        continue;
      }

      if (forced[i]) {
        // we know for sure that we want this column, so evaluate the
        // subset and set it in the environment
        SEXP expr = PROTECT(get_subset_expr(i, indices));
        SEXP result = PROTECT(Rcpp_eval(expr, R_BaseEnv));
        Rf_defineVar(
          Rf_installChar(names[i]),
          result,
          mask_bindings
        );
        UNPROTECT(2);
      }

    }
  }

  void update_promise_index(promise& p, const Index& indices) {
    SEXP code = p.code();
    SETCADDR(code, indices);
  }

};

}

#endif

