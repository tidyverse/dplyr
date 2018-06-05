#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <Rcpp.h>
#include <tools/utils.h>

namespace dplyr {

template <typename Data, typename Subsets, typename Index>
class DataMask {
public:
  DataMask(const Data& data_, Subsets& subsets_, const Rcpp::Environment& env_, Rcpp::Environment hybrid_functions_):
    data(data_),
    subsets(subsets_),
    env(env_),

    mask_promises(child_env(env)),
    mask_bottom(child_env(mask_promises)),

    promises(subsets.size()),
    names(subsets.get_variable_names().get_vector()),
    hybrids(4),
    hybrid_functions(hybrid_functions_)
  {
    mask_bottom[".data"] = internal::rlang_api().as_data_pronoun(mask_promises);
    overscope = internal::rlang_api().new_data_mask(mask_bottom, mask_promises, env);

    install_hybrid_functions();
  }

  SEXP eval(SEXP expr, const Index& indices) {
    subsets.clear();

    hybrid_functions["..group_size"] = indices.size();
    hybrid_functions["..group_number"] = indices.group() + 1;

    if (indices.group() == 0) {
      set_promises(indices);
    } else {
      update_promises(indices);
    }

    // evaluate the call in the overscope
    return Rcpp_eval(expr, overscope);
  }

private:
  const Data& data ;
  Subsets& subsets ;
  const Environment& env ;

  List payload ;
  Environment mask_promises;
  Environment mask_bottom;

  Environment overscope;
  std::vector<SEXP> promises;
  CharacterVector names ;
  std::vector<SEXP> hybrids;
  Environment hybrid_functions;


  SEXP child_env(SEXP parent) {
    static SEXP symb_new_env = Rf_install("new.env");
    return Rf_eval(Rf_lang3(symb_new_env, Rf_ScalarLogical(TRUE), parent), R_BaseEnv);
  }

  void set_promises(const Index& indices) {
    for (int i = 0; i < names.size(); i++) {
      install_promise(i, indices);
    }
  }

  void update_promises(const Index& indices) {
    for (int i = 0; i < names.size(); i++) {
      SEXP p = promises[i];
      // in any case it is a promise

      if (promise_was_forced(p)) {
        // it has been evaluated, install a new promise
        // would maybe be better to do either of:
        // - reset it to unforced, but SET_PRVALUE(p, R_UnboundValue) does not work
        // - promote the promise to its value and recalculate it upfront for each group
        install_promise(i, indices);
      } else {
        // otherwise just need to update the expression
        update_promise(p, indices);
      }

    }
  }

  // delayedAssign( {column_name}, x[i], baseenv(), mask_promises )
  void delayedAssign(SEXP name, SEXP variable, SEXP indices) {
    // for rowwise data, use x[[i]]
    static SEXP symb_bracket = Rcpp::traits::same_type<Data, RowwiseDataFrame>::value ? R_Bracket2Symbol : R_BracketSymbol ;

    SEXP symb_delayedAssign = Rf_install("delayedAssign");
    SEXP col_name = PROTECT(Rf_ScalarString(name));
    SEXP subset_expr = PROTECT(Rf_lang3(symb_bracket, variable, indices));
    SEXP delayedAssignCall = PROTECT(Rf_lang5(symb_delayedAssign, col_name, subset_expr, R_BaseEnv, mask_promises));
    PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
    UNPROTECT(4);
  }

  bool promise_was_forced(SEXP p) {
    return PRVALUE(p) != R_UnboundValue;
  }

  void install_promise(int i, const Index& indices) {
    SEXP name = names[i];
    SEXP column = subsets.get_variable(i);

    delayedAssign(name, column, indices);
    promises[i] = Rf_findVarInFrame(mask_promises, Rf_installChar(name));
  }

  void update_promise(SEXP p, const Index& indices) {
    SEXP code = PRCODE(p);
    SETCADDR(code, indices);
  }

  void install_hybrid_functions() {
    mask_bottom["n"] = (SEXP)hybrid_functions["n"];
    mask_bottom["row_number"] = (SEXP)hybrid_functions["row_number"];
    mask_bottom["group_indices"] = (SEXP)hybrid_functions["group_indices"];
    mask_bottom["ntile"] = (SEXP)hybrid_functions["ntile"];
  }

};

}

#endif
