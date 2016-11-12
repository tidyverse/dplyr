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
      call(clone(call_)), indices(NULL), subsets(subsets_), env(env_), has_active_env(false)
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
    void provide_active_env() {
      // TODO: Use a flag to detect if any new names have been added since the last call
      CharacterVector names = subsets.get_variable_names();
      CharacterVector new_names = Rcpp::setdiff(names, active_names);

      if (new_names.length() == 0)
        return;

      if (!has_active_env) {
        active_env = env.new_child(true);
        has_active_env = true;
      }

      populate_env_symbol(active_env, names, &GroupedHybridCall::hybrid_get_callback, PAYLOAD(reinterpret_cast<void*>(this)));

      active_names = active_env.ls(true);
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
      LOG_INFO << type2name(call);
      Call simplified_call = Rf_duplicate(call);
      while (simplified(simplified_call)) {}

      LOG_INFO << type2name(simplified_call);
      if (TYPEOF(simplified_call) == LANGSXP) {
        LOG_VERBOSE << "performing hybrid evaluation";
        provide_active_env();
        return Rcpp_eval(simplified_call, active_env);
      } else if (TYPEOF(simplified_call) == SYMSXP) {
        if (subsets.count(simplified_call)) {
          return subsets.get(simplified_call, get_indices());
        }
        return env.find(CHAR(PRINTNAME(simplified_call)));
      }
      return simplified_call;
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
    const Call call;
    const SlicingIndex* indices;
    Subsets& subsets;
    Environment env, active_env;
    CharacterVector active_names;
    bool has_active_env;
  };

}
#endif
