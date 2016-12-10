#ifndef dplyr_GroupedCallProxy_H
#define dplyr_GroupedCallProxy_H

#include <dplyr/get_column.h>

#include <dplyr/Hybrid.h>

#include <dplyr/Result/CallElementProxy.h>
#include <dplyr/Result/LazyGroupedSubsets.h>
#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/GroupedHybridCall.h>

namespace dplyr {

  template <typename Data = GroupedDataFrame, typename Subsets = LazyGroupedSubsets>
  class GroupedCallProxy {
  public:
    typedef GroupedHybridCall<Subsets> HybridCall;

    GroupedCallProxy(const Rcpp::Call& call_, const Subsets& subsets_, const Environment& env_) :
      subsets(subsets_), proxies()
    {
      set_call(call_);
      set_env(env_);
    }

    GroupedCallProxy(const Rcpp::Call& call_, const Data& data_, const Environment& env_) :
      subsets(data_), proxies()
    {
      set_call(call_);
      set_env(env_);
    }

    GroupedCallProxy(const Data& data_, const Environment& env_) :
      subsets(data_), proxies()
    {
      set_env(env_);
    }

    GroupedCallProxy(const Data& data_) :
      subsets(data_), proxies()
    {}

    ~GroupedCallProxy() {}

  public:
    SEXP eval() {
      return get(NaturalSlicingIndex(subsets.nrows()));
    }

    SEXP get(const SlicingIndex& indices) {
      subsets.clear();

      return get_hybrid_call()->eval(indices);
    }

    HybridCall* get_hybrid_call() {
      if (!hybrid_call) {
        hybrid_call.reset(new HybridCall(call, subsets, env));
      }

      return hybrid_call.get();
    }

    void set_call(SEXP call_) {
      proxies.clear();
      hybrid_call.reset();
      call = call_;
    }

    inline void set_env(SEXP env_) {
      env = env_;
      hybrid_call.reset();
    }

    void input(Symbol name, SEXP x) {
      subsets.input(name, x);
      hybrid_call.reset();
    }

    inline int nsubsets() const {
      return subsets.size();
    }

    inline bool has_variable(SEXP symbol) const {
      return subsets.count(symbol);
    }

    inline SEXP get_variable(Rcpp::String name) const {
      return subsets.get_variable(Rf_installChar(name.get_sexp()));
    }

    inline bool is_constant() const {
      return TYPEOF(call) != LANGSXP && Rf_length(call) == 1;
    }

  private:
    Rcpp::Call call;
    Subsets subsets;
    std::vector<CallElementProxy> proxies;
    Environment env;
    boost::scoped_ptr<HybridCall> hybrid_call;

  };

}

#endif
