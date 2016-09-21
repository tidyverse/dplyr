#ifndef dplyr_GroupedCallReducer_H
#define dplyr_GroupedCallReducer_H

#include <dplyr/Result/CallbackProcessor.h>
#include <dplyr/Result/GroupedCallProxy.h>

namespace dplyr {

  template <typename Data, typename Subsets>
  class GroupedCallReducer : public CallbackProcessor< GroupedCallReducer<Data,Subsets> > {
  public:
    GroupedCallReducer(Rcpp::Call call, const Subsets& subsets, const Environment& env):
      proxy(call, subsets, env)
    {
    }

    virtual ~GroupedCallReducer() {};

    inline SEXP process_chunk(const SlicingIndex& indices) {
      return proxy.get(indices);
    }

  private:
    GroupedCallProxy<Data, Subsets> proxy;
  };

} // namespace dplyr

#endif
