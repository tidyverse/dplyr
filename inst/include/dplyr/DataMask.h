#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/Result/LazyGroupedSubsets.h>

namespace dplyr {

class promise {
public:

  promise() :
    name(R_NilValue),
    symb_name(R_NilValue),
    env(R_NilValue)
  {}

  promise(SEXP name_, SEXP env_, SEXP expr) :
    name(Rf_ScalarString(name_)),
    symb_name(Rf_installChar(name_)),
    env(env_)
  {
    install(expr);
  }

  inline void install(SEXP expr) {
    delayedAssign(expr);
    prom = Rf_findVarInFrame(env, symb_name);
  }

  inline bool was_forced() {
    return PRVALUE(prom) != R_UnboundValue;
  }

  inline SEXP code() {
    return PRCODE(prom);
  }

private:
  SEXP name;
  SEXP symb_name;
  SEXP env;
  SEXP prom;

  // delayedAssign( name, call, eval.env = baseenv(), assign_env = env )
  void delayedAssign(SEXP expr) {
    static SEXP symb_delayedAssign = Rf_install("delayedAssign");
    SEXP delayedAssignCall = PROTECT(Rf_lang5(symb_delayedAssign, name, expr, R_BaseEnv, env));
    PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
    UNPROTECT(2);
  }

};

inline SEXP child_env(SEXP parent) {
  static SEXP symb_new_env = Rf_install("new.env");
  return Rf_eval(Rf_lang3(symb_new_env, Rf_ScalarLogical(TRUE), parent), R_BaseEnv);
}

// this class deals with the hybrid functions that are installed in the hybrids environment in the data mask
template <typename Data>
class DataMask_bottom {
public:

  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

  DataMask_bottom(SEXP parent_env, Rcpp::Environment hybrid_functions_):
    mask_bottom(child_env(parent_env)),
    hybrid_functions(hybrid_functions_)
  {
    mask_bottom["n"] = (SEXP)hybrid_functions["n"];
    mask_bottom["row_number"] = (SEXP)hybrid_functions["row_number"];
    mask_bottom["group_indices"] = (SEXP)hybrid_functions["group_indices"];
    mask_bottom["ntile"] = (SEXP)hybrid_functions["ntile"];
  }

  void update(const Index& indices) {
    hybrid_functions["..group_size"] = indices.size();
    hybrid_functions["..group_number"] = indices.group() + 1;
  }

  inline operator SEXP() {
    return mask_bottom ;
  }

  inline void set_data_pronoun(SEXP bindings) {
    mask_bottom[".data"] = internal::rlang_api().as_data_pronoun(bindings);
  }

private:
  Environment mask_bottom;
  Environment hybrid_functions;
};

// in the general case (for grouped and rowwise), the bindings
// environment contains promises of the subsets
template <typename Data>
class DataMask_bindings {
public:
  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

  DataMask_bindings(SEXP parent_env, Subsets& subsets_) :
    mask_bindings(child_env(parent_env)),
    subsets(subsets_),
    promises(subsets.size())
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
    return Rf_lang3(symb_bracket, subsets.get_variable(i), indices);
  }

  void set_promises(const Index& indices) {
    CharacterVector names = subsets.get_variable_names().get_vector();
    int n = names.size();
    for (int i = 0; i < n; i++) {
      promises[i] = promise(names[i], mask_bindings, get_subset_expr(i, indices));
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

// in the NaturalDataFrame case, we can directly install columns in the bindings environment
template <>
class DataMask_bindings<NaturalDataFrame> {
public:
  typedef LazySplitSubsets<NaturalDataFrame> Subsets;

  DataMask_bindings(SEXP parent_env, Subsets& subsets) :
    mask_bindings(child_env(parent_env))
  {
    CharacterVector names = subsets.get_variable_names().get_vector();
    int n = names.size();
    for (int i = 0; i < n; i++) {
      Rf_defineVar(Rf_installChar(names[i]), subsets.get_variable(i), mask_bindings);
    }
  }

  void update(const NaturalSlicingIndex&) {}

  inline operator SEXP() {
    return mask_bindings;
  }

private:
  Environment mask_bindings;

};


// the data mask is made of two environments
// - the `bindings` environment that maps symbols to subsets of columns from the data frame
// - the `hybrids` environment that contains functions such as `n()`
template <typename Data>
class DataMask {
public:
  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

  DataMask(Subsets& subsets, const Rcpp::Environment& env, Rcpp::Environment hybrid_functions_):
    bindings(env, subsets),
    hybrids(bindings, hybrid_functions_),
    overscope(internal::rlang_api().new_data_mask(bindings, hybrids, env))
  {}

  SEXP eval(SEXP expr, const Index& indices) {
    // update both components of the data mask
    hybrids.update(indices);
    bindings.update(indices);
    hybrids.set_data_pronoun(bindings);

    // evaluate the call in the overscope
    return Rcpp_eval(expr, overscope);
  }

private:

  // bindings for columns in the data frame
  DataMask_bindings<Data> bindings;

  // hybrid functions (n, ...)
  DataMask_bottom<Data> hybrids;

  Environment overscope;
};

}

#endif
