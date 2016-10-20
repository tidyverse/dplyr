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

    GroupedCallProxy(Call call_, const Subsets& subsets_, const Environment& env_) :
      call(call_), subsets(subsets_), proxies(), env(env_)
    {
      set_call(call);
    }

    GroupedCallProxy(const Rcpp::Call& call_, const Data& data_, const Environment& env_) :
      call(call_), subsets(data_), proxies(), env(env_)
    {
      set_call(call);
    }

    GroupedCallProxy(const Data& data_, const Environment& env_) :
      subsets(data_), proxies(), env(env_)
    {}

    GroupedCallProxy(const Data& data_) :
      subsets(data_), proxies()
    {}

    ~GroupedCallProxy() {}

    SEXP get(const SlicingIndex& indices) {
      subsets.clear();

      if (TYPEOF(call) == LANGSXP) {
        LOG_VERBOSE << "performing hybrid evaluation";
        HybridCall hybrid_eval(call, indices, subsets, env);
        return hybrid_eval.eval();
      } else if (TYPEOF(call) == SYMSXP) {
        if (subsets.count(call)) {
          return subsets.get(call, indices);
        }
        return env.find(CHAR(PRINTNAME(call)));
      } else {
        // all other types that evaluate to themselves
        return call;
      }
    }

    SEXP eval() {
      return get(NaturalSlicingIndex(subsets.nrows()));
    }

    void set_call(SEXP call_) {
      proxies.clear();
      call = call_;
    }

    void input(Symbol name, SEXP x) {
      subsets.input(name, x);
    }

    inline int nsubsets() {
      return subsets.size();
    }

    inline SEXP get_variable(Rcpp::String name) const {
      return subsets.get_variable(Rf_installChar(name.get_sexp()));
    }

    inline bool is_constant() const {
      return TYPEOF(call) != LANGSXP && Rf_length(call) == 1;
    }

    inline SEXP get_call() const {
      return call;
    }

    inline bool has_variable(SEXP symbol) const {
      return subsets.count(symbol);
    }

    inline void set_env(SEXP env_) {
      env = env_;
    }

    Rcpp::Call call;
    Subsets subsets;
    std::vector<CallElementProxy> proxies;
    Environment env;

  };

}

#endif
