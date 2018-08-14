#ifndef dplyr_DataMask_bindings_promises_H
#define dplyr_DataMask_bindings_promises_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/subset/LazyGroupedSubsets.h>
#include <dplyr/promise.h>

namespace dplyr {

// in the general case (for grouped and rowwise), the bindings
// environment contains promises of the subsets
template <typename Data>
class DataMask_bindings_promises {
public:
  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

  DataMask_bindings_promises(SEXP parent_env, Subsets& subsets_) :
    mask_bindings(child_env(parent_env)),
    subsets(subsets_),
    promises()
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
  Subsets& subsets ;
  std::vector<promise> promises;

  inline SEXP get_subset_expr(int i, const Index& indices) {
    static SEXP symb_bracket = Rcpp::traits::same_type<Data, RowwiseDataFrame>::value ? R_Bracket2Symbol : R_BracketSymbol ;
    if (subsets.is_summary(i)) {
      return Rf_lang3(symb_bracket, subsets.get_variable(i), Rf_ScalarInteger(indices.group() + 1));
    } else {
      return Rf_lang3(symb_bracket, subsets.get_variable(i), indices);
    }
  }

  void set_promises(const Index& indices) {
    CharacterVector names = subsets.get_variable_names().get_vector();
    int n = names.size();
    promises.reserve(n);
    for (int i = 0; i < n; i++) {
      promises.push_back(promise(names[i], mask_bindings, get_subset_expr(i, indices)));
    }
  }

  void update_promises(const Index& indices) {
    for (int i = 0; i < subsets.size(); i++) {
      promise& p = promises[i];

      if (p.was_forced()) {
        // it has been evaluated, install a new promise
        // would maybe be better to do either of:
        // - reset it to unforced, but SET_PRVALUE(p, R_UnboundValue) does not work and gives this error: Evaluation error: 'rho' must be an environment not NULL: detected in C-level eval.
        // - promote the promise to its value and recalculate it upfront for each group

        p.install(get_subset_expr(i, indices));
      } else {
        // otherwise just need to update the expression
        update_promise_index(p, indices);
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

