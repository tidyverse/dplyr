#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/Result/LazyGroupedSubsets.h>
#include <dplyr/DataMask_bindings_promises.h>

namespace dplyr {

template <typename Data>
class DataMask_bindings {
public:
  typedef typename Data::slicing_index Index ;
  typedef LazySplitSubsets<Data> Subsets;

  DataMask_bindings(SEXP parent_env, Subsets& subsets) :
    impl(parent_env, subsets)
  {}

  void update(const Index& indices) {
    impl.update(indices);
  }

  inline operator SEXP() {
    return impl;
  }

private:
  DataMask_bindings_promises<Data> impl ;
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
      // this handles both the normal and summarised case (via recycling rules)
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

  DataMask(Subsets& subsets, const Rcpp::Environment& env):
    bindings(env, subsets),
    overscope(internal::rlang_api().new_data_mask(bindings, bindings, env))
  {
    overscope[".data"] = internal::rlang_api().as_data_pronoun(bindings);
  }

  SEXP eval(SEXP expr, const Index& indices) {
    // update the bindings and the data context variables
    bindings.update(indices);

    // these are used by n(), ...
    overscope["..group_size"] = indices.size();
    overscope["..group_number"] = indices.group() + 1;

    // evaluate the call in the overscope
    return Rcpp_eval(expr, overscope);
  }

private:

  // bindings for columns in the data frame
  DataMask_bindings<Data> bindings;

  Environment overscope;
};

}

#endif
