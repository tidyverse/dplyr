#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

#include <tools/Call.h>
#include <tools/utils.h>

#include <dplyr/Result/Result.h>

#include <bindrcpp.h>

namespace dplyr {

SEXP dplyr_object(const char* name);


class IHybridCallback {
public:
  virtual ~IHybridCallback();

public:
  virtual SEXP get_subset(const SymbolString& name) const = 0;
};

// The GroupedHybridEnv class provides the overscope/data mask upon request,
// which forwards accesses to variables to IHybridCallback::get_subset()
// for an IHybridCallback object provided externally via a shared_ptr<>.
class GroupedHybridEnv {
  // Objects of class HybridCallbackWeakProxy are owned by an XPtr that is
  // buried in a closure maintained by bindr. We have no control of their
  // lifetime. They are connected to an IHybridCallback via a weak_ptr<>
  // constructed from a shared_ptr<>), to which all calls to get_subset()
  // are forwarded. If the underlying object has been destroyed (which can
  // happen if the data mask leaks and survives the dplyr verb,
  // sometimes unintentionally), the weak pointer cannot be locked, and
  // get_subset() returns NULL with a warning (#3318).
  class HybridCallbackWeakProxy : public IHybridCallback {
  public:
    HybridCallbackWeakProxy(boost::shared_ptr<const IHybridCallback> real_);

  public:
    SEXP get_subset(const SymbolString& name) const;

    virtual ~HybridCallbackWeakProxy();

  private:
    boost::weak_ptr<const IHybridCallback> real;
  };

public:
  GroupedHybridEnv(const CharacterVector& names_, const Environment& env_, boost::shared_ptr<const IHybridCallback> callback_);
  ~GroupedHybridEnv();

public:
  const Environment& get_overscope() const;

private:
  void provide_overscope() const;

  static SEXP hybrid_get_callback(const String& name, List payload);

private:
  const CharacterVector names;
  const Environment env;
  boost::shared_ptr<const IHybridCallback> callback;

  mutable Environment overscope;
  mutable Environment mask_active;
  mutable Environment mask_bottom;
  mutable bool has_overscope;
};

// This class replaces calls to hybrid handlers by literals representing the result.
class GroupedHybridCall {
public:
  GroupedHybridCall(const Call& call_, const ILazySubsets& subsets_, const Environment& env_);

public:
  Call simplify(const SlicingIndex& indices) const;

private:
  bool simplified(Call& call) const;
  bool replace(SEXP p) const;

  const SlicingIndex& get_indices() const;
  void set_indices(const SlicingIndex& indices_) const;
  void clear_indices() const;

private:
  // Initialization
  const Call original_call;
  const ILazySubsets& subsets;
  const Environment env;

private:
  // State
  mutable const SlicingIndex* indices;
};

// The GroupedHybridEval class evaluates expressions for each group.
// It implements IHybridCallback to handle requests for the value of
// a variable.
class GroupedHybridEval : public IHybridCallback {
  // Objects of HybridCallbackProxy are owned by GroupedHybridEval and
  // held with a shared_ptr<> to support weak references. They simply
  // forward to the enclosing GroupedHybridEval via the IHybridCallback
  // interface.
  class HybridCallbackProxy : public IHybridCallback {
  public:
    HybridCallbackProxy(const IHybridCallback* real_);
    virtual ~HybridCallbackProxy();

  public:
    SEXP get_subset(const SymbolString& name) const;

  private:
    const IHybridCallback* real;
  };

public:
  GroupedHybridEval(const Call& call_, const ILazySubsets& subsets_, const Environment& env_);

  const SlicingIndex& get_indices() const;

public: // IHybridCallback
  SEXP get_subset(const SymbolString& name) const;

public:
  SEXP eval(const SlicingIndex& indices_);

private:
  void set_indices(const SlicingIndex& indices_);
  void clear_indices();
  SEXP eval_with_indices();

private:
  const SlicingIndex* indices;
  const ILazySubsets& subsets;
  Environment env;
  boost::shared_ptr<IHybridCallback> proxy;
  const GroupedHybridEnv hybrid_env;
  const GroupedHybridCall hybrid_call;
};


} // namespace dplyr

#endif
