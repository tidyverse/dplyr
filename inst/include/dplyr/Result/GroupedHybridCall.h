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
    GroupedHybridCall(const Call& call_, Subsets& subsets_, const Environment& env_) :
      call(call_), indices(NULL), subsets(subsets_), env(env_), has_eval_env(false)
    {
      LOG_VERBOSE;
    }

  public:
    SEXP eval(const SlicingIndex& indices_) {
      set_indices(indices_);
      SEXP ret = eval_with_indices();
      clear_indices();
      return ret;
    }

  private:
    const Environment& get_eval_env() {
      provide_eval_env();
      return eval_env;
    }

    void provide_eval_env() {
      if (has_eval_env)
        return;

      // bindr::populate_env() does less work in R, I'm assuming that creating the environment from C++ will be faster
      Environment active_env = env.new_child(true);

      CharacterVector names = subsets.get_variable_names();
      if (names.length() > 0) {
        populate_env_symbol(
          active_env, names,
          &GroupedHybridCall::hybrid_get_callback, PAYLOAD(reinterpret_cast<void*>(this)));
      }

      eval_env = active_env.new_child(true);
      eval_env[".data"] = active_env;
      eval_env[".env"] = env;

      has_eval_env = true;
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

    const SlicingIndex& get_indices() const {
      return *indices;
    }

    void set_indices(const SlicingIndex& indices_) {
      indices = &indices_;
    }

    void clear_indices() {
      indices = NULL;
    }

    SEXP eval_with_indices() {
      Call call_ = clone(call);
      while (simplified(call_)) {}
      return eval_with_indices_simplified(call_);
    }

    SEXP eval_with_indices_simplified(const Call& call) {
      LOG_INFO << type2name(call);
      if (TYPEOF(call) == LANGSXP) {
        LOG_VERBOSE << "performing evaluation in eval_env";
        return Rcpp_eval(call, get_eval_env());
      } else if (TYPEOF(call) == SYMSXP) {
        if (subsets.count(call)) {
          return subsets.get(call, get_indices());
        }
        return env.find(CHAR(PRINTNAME(call)));
      }
      return call;
    }

    bool simplified(Call& call) {
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
    const Call& call;
    const SlicingIndex* indices;
    Subsets& subsets;
    Environment env, eval_env;
    bool has_eval_env;
  };

}
#endif
