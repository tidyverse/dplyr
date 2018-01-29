#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

#include <boost/scoped_ptr.hpp>

#include <tools/Call.h>
#include <tools/utils.h>

#include <dplyr/Result/Result.h>

#include <bindrcpp.h>

namespace dplyr {

inline static
SEXP dplyr_object(const char* name) {
  static Environment dplyr = Rcpp::Environment::namespace_env("dplyr");
  return dplyr[name];
}


class IHybridCallback {
protected:
  virtual ~IHybridCallback() {}

public:
  virtual SEXP get_subset(const SymbolString& name) const = 0;
};


class GroupedHybridEnv {
public:
  GroupedHybridEnv(const CharacterVector& names_, const Environment& env_, const IHybridCallback* callback_) :
    names(names_), env(env_), callback(callback_), has_overscope(false)
  {
    LOG_VERBOSE;
  }

  ~GroupedHybridEnv() {
    if (has_overscope) {
      // We need to call into R because there is no C API for removing
      // bindings from environments
      static Function env_wipe = dplyr_object("env_wipe");
      env_wipe(mask_active);
      env_wipe(mask_bottom);
    }
  }

public:
  const Environment& get_overscope() const {
    provide_overscope();
    return overscope;
  }

private:
  void provide_overscope() const {
    if (has_overscope)
      return;

    // Environment::new_child() performs an R callback, creating the environment
    // in R should be slightly faster
    mask_active =
      create_env_string(
        names, &GroupedHybridEnv::hybrid_get_callback,
        PAYLOAD(const_cast<void*>(reinterpret_cast<const void*>(callback))), env);

    // If bindr (via bindrcpp) supported the creation of a child environment, we could save the
    // call to Rcpp_eval() triggered by mask_active.new_child()
    mask_bottom = mask_active.new_child(true);
    mask_bottom[".data"] = internal::rlang_as_data_pronoun(mask_active);

    // Install definitions for formula self-evaluation and unguarding
    overscope = internal::rlang_new_data_mask(mask_bottom, mask_active, env);

    has_overscope = true;
  }

  static SEXP hybrid_get_callback(const String& name, bindrcpp::PAYLOAD payload) {
    LOG_VERBOSE;
    IHybridCallback* callback_ = reinterpret_cast<IHybridCallback*>(payload.p);
    return callback_->get_subset(SymbolString(name));
  }

private:
  const CharacterVector names;
  const Environment env;
  const IHybridCallback* callback;

  mutable Environment overscope;
  mutable Environment mask_active;
  mutable Environment mask_bottom;
  mutable bool has_overscope;
};


class GroupedHybridCall {
public:
  GroupedHybridCall(const Call& call_, const ILazySubsets& subsets_, const Environment& env_) :
    original_call(call_), subsets(subsets_), env(env_)
  {
    LOG_VERBOSE;
  }

public:
  // FIXME: replace the search & replace logic with overscoping
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
    hybrid_env(subsets_.get_variable_names().get_vector(), env_, this),
    hybrid_call(call_, subsets_, env_)
  {
    LOG_VERBOSE;
  }

  const SlicingIndex& get_indices() const {
    return *indices;
  }

public: // IHybridCallback
  SEXP get_subset(const SymbolString& name) const {
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
      LOG_VERBOSE << "performing evaluation in overscope";
      return Rcpp_eval(call, hybrid_env.get_overscope());
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


} // namespace dplyr

#endif
