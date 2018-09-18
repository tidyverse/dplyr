#ifndef dplyr_DataMask_H
#define dplyr_DataMask_H

#include <tools/SymbolMap.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/visitors/subset/column_subset.h>

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

namespace dplyr {

template <class SlicedTibble> class DataMask;
template <class SlicedTibble> class DataMaskProxy;
template <class SlicedTibble> class DataMaskWeakProxy;

// Manages a single binding, used by the DataMask classes below
template <typename SlicedTibble>
struct ColumnBinding {
private:
  // is this a summary binding, i.e. does it come from summarise
  bool summary;

  // symbol of the binding
  SEXP symbol;

  // data. it is own either by the original data frame or by the
  // accumulator, so no need for additional protection here
  SEXP data;

public:

  ColumnBinding(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_)
  {}

  // the active binding function calls eventually calls DataMask<>::materialize
  // which calls this method
  inline SEXP get(
    const typename SlicedTibble::slicing_index& indices,
    SEXP mask_resolved)
  {
    return materialize(indices, mask_resolved);
  }

  inline void clear(SEXP mask_resolved) {
    Rf_defineVar(symbol, R_UnboundValue, mask_resolved);
  }

  // summary accessor
  bool is_summary() const {
    return summary;
  }

  // data accessor
  inline SEXP get_data() const {
    return data;
  }

  void rm() {
    data = R_NilValue;
  }

  bool is_null() const {
    return data == R_NilValue;
  }

  // update the resolved binding in mask_resolved withe the given indices
  // DataMask<> only calls this on previously materialized bindings
  // this is only used for its side effect of storing the result
  // in the right environment
  inline void update_indices(
    const typename SlicedTibble::slicing_index& indices,
    SEXP mask_resolved)
  {
    materialize(indices, mask_resolved);
  }

  // setup the active binding with a function made by dplyr:::.active_binding_fun
  //
  // .active_binding_fun holds the position and a pointer to the DataMask
  inline void install(
    SEXP mask_active,
    SEXP mask_resolved,
    int pos,
    boost::shared_ptr< DataMaskProxy<SlicedTibble> >& data_mask_proxy
  ) {
    static Function active_binding_fun(
      ".active_binding_fun",
      Rcpp::Environment::namespace_env("dplyr")
    );

    // external pointer to the weak proxy of the data mask
    // eventually this calls back to the reak DataMask
    XPtr< DataMaskWeakProxy<SlicedTibble> > weak_proxy(
      new DataMaskWeakProxy<SlicedTibble>(data_mask_proxy)
    );

    R_MakeActiveBinding(
      // the name of the binding
      symbol,

      // the function
      active_binding_fun(pos, weak_proxy),

      // where to set it up as an active binding
      mask_active
    );
  }

  // nothing to do here, this is only relevant for ColumnBinding<NaturalDataFrame>
  inline void update(SEXP mask_active, SEXP mask_resolved) {}

  // remove the binding in the mask_active environment
  // so that standard evaluation does not find it
  //
  // this is a fairly expensive callback to R, but it only happens
  // when we use the syntax <column> = NULL
  inline void detach(SEXP mask_active, SEXP mask_resolved) {
    Language("rm", symbol, _["envir"] = mask_active).eval(R_BaseEnv);
  }

private:

  // materialize the subset of data using column_subset
  // and store the result in the given environment
  inline SEXP materialize(
    const typename SlicedTibble::slicing_index& indices,
    SEXP mask_resolved)
  {

    // materialize
    Shield<SEXP> value(summary ?
                       column_subset(data, RowwiseSlicingIndex(indices.group())) :
                       column_subset(data, indices)
                      );

    // store it in the mask_resolved environment
    Rf_defineVar(symbol, value, mask_resolved);
    return value;
  }

};

// special case for NaturalDataFrame because there is no need
// for active bindings in this case
//
// - if this is a summary, it is length 1 and can be returned as is
// - otherwise, it can also be returned as is because the
//   NaturalDataFrame::slicing_index always want the entire column
template <>
struct ColumnBinding<NaturalDataFrame> {
public:
  ColumnBinding(bool summary_, SEXP symbol_, SEXP data_) :
    summary(summary_),
    symbol(symbol_),
    data(data_)
  {}

  // nothing to do here, this is never actually used
  inline SEXP get(
    const NaturalDataFrame::slicing_index& indices,
    SEXP mask_resolved)
  {
    return data;
  }

  inline void clear(SEXP mask_resolved) {}

  bool is_summary() const {
    return summary;
  }

  inline SEXP get_data() const {
    return data;
  }

  void rm() {
    data = R_NilValue;
  }

  bool is_null() const {
    return data == R_NilValue;
  }

  // never used
  inline void update_indices(
    const NaturalDataFrame::slicing_index& /* indices */,
    SEXP /* env */)
  {}

  // TODO: when .data knows how to look ancestry, this should use mask_resolved instead
  //
  // it does not really install an active binding because there is no need for that
  inline void install(
    SEXP mask_active,
    SEXP mask_resolved,
    int /* pos */,
    boost::shared_ptr< DataMaskProxy<NaturalDataFrame> >& /* data_mask_proxy */)
  {
    Rf_defineVar(symbol, data, mask_active);
  }

  // update the (not so active) binding
  // this is used by cases like
  // mutate( x = fun(x) )
  inline void update(SEXP mask_active, SEXP mask_resolved) {
    Rf_defineVar(symbol, data, mask_active);
  }

  // remove the binding in the mask_active environment
  // so that standard evaluation does not find it
  inline void detach(SEXP mask_active, SEXP mask_resolved) {
    Language("rm", symbol, _["envir"] = mask_active).eval();
  }

private:

  bool summary;
  SEXP symbol;
  SEXP data;
};

// base class for instantiations of the DataMaskWeakProxy<> template
// the base class is used when called from the active binding in R
class DataMaskWeakProxyBase {
public:
  virtual ~DataMaskWeakProxyBase() {}

  virtual SEXP materialize(int idx) = 0;
};

// This holds a pointer to a real DataMask<>
//
// A DataMaskProxy<> is only used in a shared_ptr<DataMaskProxy<>>
// that is held by the DataMask<>
//
// This is needed because weak_ptr needs a shared_ptr
template <typename SlicedTibble>
class DataMaskProxy {
private:
  DataMask<SlicedTibble>* real;

public:
  DataMaskProxy(DataMask<SlicedTibble>* real_) : real(real_) {}

  SEXP materialize(int idx) {
    return real->materialize(idx);
  }
};

// This holds a weak_ptr to a DataMaskProxy<SlicedTibble> that ultimately
// calls back to the DataMask if it is still alive
template <typename SlicedTibble>
class DataMaskWeakProxy : public DataMaskWeakProxyBase {
private:
  boost::weak_ptr< DataMaskProxy<SlicedTibble> > real;

public:
  DataMaskWeakProxy(boost::shared_ptr< DataMaskProxy<SlicedTibble> > real_) :
    real(real_)
  {}

  virtual SEXP materialize(int idx) {
    if (boost::shared_ptr< DataMaskProxy<SlicedTibble> > lock = real.lock()) {
      return lock.get()->materialize(idx);
    } else {
      warning("Hybrid callback proxy out of scope");
      return R_NilValue;
    }
  }
};

// typical use
//
// // a tibble (grouped, rowwise, or natural)
// SlicedTibble data(...) ;
// DataMask<SlicedTibble> mask(data);
//
// if using hybrid evaluation, we only need to check for existence of variables
// in the map with mask.maybe_get_subset_binding(SymbolString)
// This returns a ColumnBinding<SlicedTibble>
//
// if using standard evaluation, first the data_mask must be rechain()
// so that it's top environment has the env as a parent
//
// data_mask.rechain(SEXP env) ;
//
// this effectively sets up the R data mask, so that we can evaluate r expressions
// so for each group:
//
// data_mask.update(indices)
//
// this keeps a track of the current indices
// - for bindings that have not been resolved before, nothing needs to happen
//
// - for bindings that were previously resolved (as tracked by the
//   materialized vector) they are re-materialized pro-actively
//   in the resolved environment
template <class SlicedTibble>
class DataMask {
  typedef typename SlicedTibble::slicing_index slicing_index;

public:

  // constructor
  // - fills the symbol map quickly (no hashing), assuming
  //   the names are all different
  // - fills the column_bindings vector
  //
  // - delays setting up the environment until needed
  DataMask(const SlicedTibble& gdf) :
    column_bindings(),
    symbol_map(gdf.data().size(), gdf.data().names()),
    active_bindings_ready(false),
    proxy(new DataMaskProxy<SlicedTibble>(this))
  {
    const DataFrame& data = gdf.data();
    CharacterVector names = data.names();
    int n = data.size();
    LOG_INFO << "processing " << n << " vars: " << names;

    // install the column_bindings without lookups in symbol_map
    // i.e. not using input_column
    for (int i = 0; i < n; i++) {
      column_bindings.push_back(
        ColumnBinding<SlicedTibble>(
          false, SymbolString(names[i]).get_symbol(),
          data[i]
        )
      );
    }

    previous_group_size = get_context_env()["..group_size"];
    previous_group_number = get_context_env()["..group_number"];
  }

  ~DataMask() {
    get_context_env()["..group_size"] = previous_group_size;
    get_context_env()["..group_number"] = previous_group_number;
  }

  // returns a pointer to the ColumnBinding if it exists
  // this is mostly used by the hybrid evaluation
  const ColumnBinding<SlicedTibble>*
  maybe_get_subset_binding(const SymbolString& symbol) const {
    int pos = symbol_map.find(symbol);
    if (pos >= 0 && !column_bindings[pos].is_null()) {
      return &column_bindings[pos];
    } else {
      return 0;
    }
  }

  // remove this variable from the environments
  void rm(const SymbolString& symbol) {
    int idx = symbol_map.find(symbol);

    if (active_bindings_ready) {
      column_bindings[idx].detach(mask_active, mask_resolved);
    }

    // so that hybrid evaluation does not find it
    // see maybe_get_subset_binding above
    column_bindings[idx].rm();
  }

  // add a new binding, used by mutate
  void input_column(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, false, x);
  }

  // add a new summarised variable, used by summarise
  void input_summarised(const SymbolString& symbol, SEXP x) {
    input_impl(symbol, true, x);
  }

  // the number of bindings
  int size() const {
    return column_bindings.size();
  }

  // call this before treating new expression with standard
  // evaluation in its environment: parent_env
  //
  // no need to call this when treating the expression with hybrid evaluation
  // this is why the setup if the environments is lazy,
  // as we might not need them at all
  void rechain(SEXP env) {
    static SEXP s_dot_env = Rf_install(".env");
    static SEXP s_dot_data = Rf_install(".data");

    if (!active_bindings_ready) {
      // the active bindings have not been used at all
      // so setup the environments ...
      mask_active = child_env(R_EmptyEnv);
      mask_resolved = child_env(mask_active);

      // ... and install the bindings
      for (size_t i = 0; i < column_bindings.size(); i++) {
        column_bindings[i].install(mask_active, mask_resolved, i, proxy);
      }

      // setup the data mask with
      //
      // bottom    : the environment with the "resolved" bindings,
      //             this is initially empty but gets filled
      //             as soon as the active binding is resolved
      //
      // top       : the environment containing active bindings.
      //
      // data_mask : where .data etc ... are installed
      data_mask = rlang::new_data_mask(
                    mask_resolved, // bottom
                    mask_active    // top
                  );

      // install the pronoun
      Rf_defineVar(s_dot_data, rlang::as_data_pronoun(mask_active), data_mask);

      active_bindings_ready = true;
    } else {

      // remove the materialized bindings from the mask_resolved environment
      for (size_t i = 0; i < materialized.size(); i++) {
        column_bindings[materialized[i]].clear(mask_resolved);
      }

      // forget about which indices are materialized
      materialized.clear();
    }

    // change the parent environment of mask_active
    SET_ENCLOS(mask_active, env);
    Rf_defineVar(s_dot_env, env, data_mask);
  }

  // get ready to evaluate an R expression for a given group
  // as identified by the indices
  void update(const slicing_index& indices) {
    // hold the current indices, as they might be needed by the active bindings
    set_current_indices(indices);

    // re-materialize the bindings that we know we need
    // because they have been used by previous groups when evaluating the same
    // expression
    for (size_t i = 0; i < materialized.size(); i++) {
      column_bindings[materialized[i]].update_indices(indices, mask_resolved);
    }
  }

  // called from the active binding, see utils-bindings.(R|cpp)
  //
  // the bindings are installed in the mask_bindings environment
  // with this R function:
  //
  // .active_binding_fun <- function(index, mask_proxy_xp){
  //   function() {
  //     materialize_binding(index, mask_proxy_xp)
  //   }
  // }
  //
  // each binding is instaled only once, the function holds:
  // - index:          the position in the column_bindings vector
  // - mask_proxy_xp : an external pointer to (a proxy to) this DataMask
  //
  //  materialize_binding is defined in utils-bindings.cpp as:
  //
  // // [[Rcpp::export]]
  // SEXP materialize_binding(
  //   int idx,
  //   XPtr<DataMaskWeakProxyBase> mask_proxy_xp)
  // {
  //   return mask_proxy_xp->materialize(idx);
  // }
  virtual SEXP materialize(int idx) {
    // materialize the subset (or just fetch it on the Natural case)
    //
    // the materialized result is stored in
    // the mask_resolved environment,
    // so we don't need to further protect `res`
    SEXP res = column_bindings[idx].get(
                 get_current_indices(), mask_resolved
               );


    // remember to pro-actievely materialize this binding on the next group
    materialized.push_back(idx);

    return res;
  }

  // evaluate expr on the subset of data given by indices
  SEXP eval(SEXP expr, const slicing_index& indices) {
    // update the bindings
    update(indices);

    // update the data context variables, these are used by n(), ...
    get_context_env()["..group_size"] = indices.size();
    get_context_env()["..group_number"] = indices.group() + 1;

    // evaluate the call in the data mask
    SEXP res = Rcpp_eval(expr, data_mask);

    return res;
  }

private:
  // forbid copy construction of this class
  DataMask(const DataMask&);
  DataMask();

  // the bindings managed by this data mask
  std::vector< ColumnBinding<SlicedTibble> > column_bindings ;

  // indices of the bdings that have been materialized
  std::vector<int> materialized ;

  // symbol map, used to retrieve a binding from its name
  SymbolMap symbol_map;

  // The 3 environments of the data mask
  Environment mask_active;  // where the active bindings live
  Environment mask_resolved; // where the resolved active bindings live
  Environment data_mask; // actual data mask, contains the .data pronoun

  // are the active bindings ready ?
  bool active_bindings_ready;

  // The current indices
  const slicing_index* current_indices;

  // previous values for group_number and group_size
  RObject previous_group_size;
  RObject previous_group_number;

  boost::shared_ptr< DataMaskProxy<SlicedTibble> > proxy;

  void set_current_indices(const slicing_index& indices) {
    current_indices = &indices;
  }

  const slicing_index& get_current_indices() {
    return *current_indices;
  }

  // input a new binding, from mutate or summarise
  void input_impl(const SymbolString& symbol, bool summarised, SEXP x) {
    // lookup in the symbol map for the position and whether it is a new binding
    SymbolMapIndex index = symbol_map.insert(symbol);

    ColumnBinding<SlicedTibble> binding(summarised, symbol.get_symbol(), x);

    if (index.origin == NEW) {
      // when this is a new variable, install the active binding
      // but only if the bindings have already been installed
      // otherwise, nothing needs to be done
      if (active_bindings_ready) {
        binding.install(mask_active, mask_resolved, index.pos, proxy);
      }

      // push the new binding at the end of the vector
      column_bindings.push_back(binding);
    } else {
      // otherwise, update it
      if (active_bindings_ready) {
        binding.update(mask_active, mask_resolved);
      }

      column_bindings[index.pos] = binding;

    }
  }

  Rcpp::Environment& get_context_env() const {
    static Rcpp::Environment context_env(
      Rcpp::Environment::namespace_env("dplyr")["context_env"]
    );
    return context_env;
  }

};

}
#endif
