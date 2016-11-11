#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

#include <boost/scoped_ptr.hpp>

#include <tools/Call.h>

#include <dplyr/Result/Result.h>

#include <bindrcpp.h>

namespace dplyr {

  template <typename Subsets>
  class GroupedHybridCall {
  public:
    GroupedHybridCall(const Call& call_, const SlicingIndex& indices_, Subsets& subsets_, const Environment& env_) :
      call(clone(call_)), indices(&indices_), subsets(subsets_), env(create_subset_env(env_))
    {
      LOG_VERBOSE;
      populate_subset_env(subsets);
      while (simplified()) {}
    }

    static Environment create_subset_env(Environment parent) {
      LOG_VERBOSE;
      return parent.new_child(true);
    }

  private:
    void populate_subset_env(const Subsets& subsets) {
      using namespace bindrcpp;

      LOG_VERBOSE;
      CharacterVector names = subsets.get_variable_names();
      populate_env_symbol(env, names, &GroupedHybridCall::hybrid_get_callback, PAYLOAD(reinterpret_cast<void*>(this)));
    }

    static SEXP hybrid_get_callback(const Symbol& name, bindrcpp::PAYLOAD payload) {
      LOG_VERBOSE;
      GroupedHybridCall* this_ = reinterpret_cast<GroupedHybridCall*>(payload.p);
      return this_->get_subset(name);
    }

    SEXP get_subset(Symbol name) {
      LOG_VERBOSE;
      return subsets.get(name, get_indices());
    }

  public:
    const SlicingIndex& get_indices() const {
      return *indices;
    }

    SEXP eval() {
      LOG_INFO << type2name(call);
      if (TYPEOF(call) == LANGSXP) {
        LOG_VERBOSE << "performing hybrid evaluation";
        return Rcpp_eval(call, env);
      } else if (TYPEOF(call) == SYMSXP) {
        if (subsets.count(call)) {
          return subsets.get(call, get_indices());
        }
        return env.find(CHAR(PRINTNAME(call)));
      }
      return call;
    }

  private:

    bool simplified() {
      LOG_VERBOSE;
      // initial
      if (TYPEOF(call) == LANGSXP) {
        boost::scoped_ptr<Result> res(get_handler(call, subsets, env));
        if (res) {
          // replace the call by the result of process
          call = res->process(get_indices());

          // no need to go any further, we simplified the top level
          return true;
        }
        return replace(CDR(call));
      }
      return false;
    }

    bool replace(SEXP p) {
      LOG_VERBOSE;
      SEXP obj = CAR(p);
      if (TYPEOF(obj) == LANGSXP) {
        boost::scoped_ptr<Result> res(get_handler(obj, subsets, env));
        if (res) {
          SETCAR(p, res->process(get_indices()));
          return true;
        }

        if (replace(CDR(obj))) return true;
      }

      if (TYPEOF(p) == LISTSXP) {
        return replace(CDR(p));
      }

      return false;
    }

  private:
    Call call;
    const SlicingIndex* indices;
    Subsets& subsets;
    const Environment env;
  };

}
#endif
