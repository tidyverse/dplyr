#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

#include <boost/scoped_ptr.hpp>

#include <tools/Call.h>

#include <dplyr/Result/Result.h>

#include <bindrcpp.h>

namespace dplyr {

  class IHybridCallback {
  protected:
    virtual ~IHybridCallback() {}

  public:
    virtual SEXP get_subset(const Symbol& name) const = 0;
  };

  class GroupedHybridEnv {
  public:
    GroupedHybridEnv(const CharacterVector& names_, const Environment& env_, const IHybridCallback* callback_) :
      names(names_), env(env_), callback(callback_), has_eval_env(false)
    {
      LOG_VERBOSE;
    }

  public:
    const Environment& get_eval_env() const {
      provide_eval_env();
      return eval_env;
    }

  private:
    void provide_eval_env() const {
      if (has_eval_env)
        return;

      // Environment::new_child() performs an R callback, creating the environment
      // in R should be slightly faster
      Environment active_env =
        create_env_symbol(
          names, &GroupedHybridEnv::hybrid_get_callback,
          PAYLOAD(const_cast<void*>(reinterpret_cast<const void*>(callback))), env);

      // If bindr (via bindrcpp) supported the creation of a child environment, we could save the
      // call to Rcpp_eval() triggered by active_env.new_child()
      eval_env = active_env.new_child(true);
      eval_env[".data"] = active_env;
      eval_env[".env"] = env;

      has_eval_env = true;
    }

    static SEXP hybrid_get_callback(const Symbol& name, bindrcpp::PAYLOAD payload) {
      LOG_VERBOSE;
      IHybridCallback* callback_ = reinterpret_cast<IHybridCallback*>(payload.p);
      return callback_->get_subset(name);
    }

  private:
    const CharacterVector names;
    const Environment env;
    const IHybridCallback* callback;

    mutable Environment eval_env;
    mutable bool has_eval_env;
  };

  class GroupedHybridCall {
  public:
    GroupedHybridCall(const Call& call_, const ILazySubsets& subsets_, const Environment& env_) :
      original_call(call_), subsets(subsets_), env(env_)
    {
      LOG_VERBOSE;
    }

  public:
    Call simplify(const SlicingIndex& indices) const {
      set_indices(indices);
      Call call = clone(original_call);
      while (simplified(call)) {}
      clear_indices();
      return call;
    }

  private:
    bool simplified(Call& call) const {
      LOG_VERBOSE;
      // initial
      if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
        boost::scoped_ptr<Result> res(get_handler(call, subsets, env));
        if (res) {
          // replace the call by the result of process
          call = res->process(get_indices());

          // no need to go any further, we simplified the top level
          return true;
        }
        if (TYPEOF(call) == LANGSXP)
          return replace(CDR(call));
      }
      return false;
    }

    bool replace(SEXP p) const {
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

    const SlicingIndex& get_indices() const {
      return *indices;
    }

    void set_indices(const SlicingIndex& indices_) const {
      indices = &indices_;
    }

    void clear_indices() const {
      indices = NULL;
    }

  private:
    // Initialization
    const Call original_call;
    const ILazySubsets& subsets;
    const Environment env;

  private:
    // State
    mutable const SlicingIndex* indices;
  };

  class GroupedHybridEval : public IHybridCallback {
  public:
    GroupedHybridEval(const Call& call_, const ILazySubsets& subsets_, const Environment& env_) :
      indices(NULL), subsets(subsets_), env(env_),
      hybrid_env(subsets_.get_variable_names(), env_, this),
      hybrid_call(call_, subsets_, env_)
    {
      LOG_VERBOSE;
    }

    const SlicingIndex& get_indices() const {
      return *indices;
    }

  public: // IHybridCallback
    SEXP get_subset(const Symbol& name) const {
      LOG_VERBOSE;
      return subsets.get(name, get_indices());
    }

  public:
    SEXP eval(const SlicingIndex& indices_) {
      set_indices(indices_);
      SEXP ret = eval_with_indices();
      clear_indices();
      return ret;
    }

  private:
    void set_indices(const SlicingIndex& indices_) {
      indices = &indices_;
    }

    void clear_indices() {
      indices = NULL;
    }

    SEXP eval_with_indices() {
      Call call = hybrid_call.simplify(get_indices());
      LOG_INFO << type2name(call);

      if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
        LOG_VERBOSE << "performing evaluation in eval_env";
        return Rcpp_eval(call, hybrid_env.get_eval_env());
      }
      return call;
    }

  private:
    const SlicingIndex* indices;
    const ILazySubsets& subsets;
    Environment env;
    const GroupedHybridEnv hybrid_env;
    const GroupedHybridCall hybrid_call;
  };

}
#endif
