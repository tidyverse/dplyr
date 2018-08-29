#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/data/LazySplitSubsets.h>

#include <bindrcpp.h>
#include <boost/shared_ptr.hpp>

namespace dplyr {

// in the general case (for grouped and rowwise), the bindings
// environment contains active bindings of the subsets
template <typename Data>
class DataMask_bindings {
public:
  typedef typename Data::slicing_index Index ;

private:

  class GroupedHybridEval {
  public:
    GroupedHybridEval(LazySplitSubsets<Data>& subsets_) :
      indices(NULL),
      subsets(subsets_),
      mask_env(R_NilValue)
    {
      LOG_VERBOSE;
    }

    const Index& get_indices() const {
      return *indices;
    }

    SEXP get_subset(const SymbolString& name) {
      return subsets.get(name, get_indices(), mask_env);
    }

    void set_indices(const Index& indices_) {
      indices = &indices_;
    }

    void set_env(SEXP env) {
      mask_env = env;
    }

  private:
    const Index* indices;
    LazySplitSubsets<Data>& subsets;
    SEXP mask_env;
  };

public:
  DataMask_bindings(SEXP parent_env, LazySplitSubsets<Data>& subsets_) :
    subsets(subsets_),
    callback(new GroupedHybridEval(subsets))
  {
    CharacterVector names = subsets.get_variable_names().get_vector();

    XPtr<GroupedHybridEval> p(callback.get(), false);
    List payload = List::create(p);

    // Environment::new_child() performs an R callback, creating the environment
    // in R should be slightly faster
    mask_active = bindrcpp::create_env_string_wrapped(
                    names, &DataMask_bindings::hybrid_get_callback,
                    payload, parent_env
                  );

    mask_resolved = mask_active.new_child(true);
    subsets.clear();
    callback->set_env(mask_resolved);
  }

  inline SEXP bottom() {
    return mask_resolved;
  }

  inline SEXP top() {
    return mask_active;
  }

  void update(const Index& indices) {
    subsets.update(indices, mask_resolved);
    callback->set_indices(indices);
  }

private:
  Environment mask_active;
  Environment mask_resolved;

  LazySplitSubsets<Data>& subsets ;
  boost::shared_ptr<GroupedHybridEval> callback;

  static SEXP hybrid_get_callback(const String& name, List payload) {
    XPtr<GroupedHybridEval> callback_ = payload[0];
    return callback_->get_subset(SymbolString(name));
  }

};

// in the NaturalDataFrame case, we can directly install columns in the bindings environment
template <>
class DataMask_bindings<NaturalDataFrame> {
public:

  DataMask_bindings(SEXP parent_env, LazySplitSubsets<NaturalDataFrame>& subsets) :
    mask_bindings(child_env(parent_env))
  {
    CharacterVector names = subsets.get_variable_names().get_vector();
    int n = names.size();
    for (int i = 0; i < n; i++) {
      // this handles both the normal and summarised case (via recycling rules)
      Rf_defineVar(Rf_install(names[i]), subsets.get_subset_data(i).get_data(), mask_bindings);
    }
  }

  void update(const NaturalSlicingIndex&) {}

  inline SEXP bottom() {
    return mask_bindings;
  }

  inline SEXP top() {
    return mask_bindings;
  }

private:
  Environment mask_bindings;

};


// the data mask handles binding names to subset of columns
template <typename Data>
class DataMask {
public:
  typedef typename Data::slicing_index Index ;

  DataMask(LazySplitSubsets<Data>& subsets, const Rcpp::Environment& env):
    bindings(env, subsets),
    overscope(internal::rlang_api().new_data_mask(bindings.bottom(), bindings.top(), env))
  {
    overscope[".data"] = internal::rlang_api().as_data_pronoun(bindings.top());
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
