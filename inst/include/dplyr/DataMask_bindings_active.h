#ifndef dplyr_DataMask_bindings_active_H
#define dplyr_DataMask_bindings_active_H

#include <Rcpp.h>
#include <tools/utils.h>
#include <dplyr/subset/LazySplitSubsets.h>

#include <boost/weak_ptr.hpp>
#include <bindrcpp.h>

namespace dplyr {

class IHybridCallback {
public:
  virtual ~IHybridCallback(){}

public:
  virtual SEXP get_subset(const SymbolString& name) const = 0;
};

// in the general case (for grouped and rowwise), the bindings
// environment contains active bindings of the subsets
template <typename Data>
class DataMask_bindings_active {
public:
  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

private:

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
    HybridCallbackWeakProxy(boost::shared_ptr<const IHybridCallback> real_):
      real(real_)
    {
      LOG_VERBOSE;
    }

  public:
    SEXP get_subset(const SymbolString& name) const {
      if (boost::shared_ptr<const IHybridCallback> lock = real.lock()) {
        return lock.get()->get_subset(name);
      }
      else {
        warning("Hybrid callback proxy out of scope");
        return R_NilValue;
      }
    }

    virtual ~HybridCallbackWeakProxy() {
      LOG_VERBOSE;
    }

  private:
    boost::weak_ptr<const IHybridCallback> real;
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
      HybridCallbackProxy(const IHybridCallback* real_) :
        real(real_)
      {
        LOG_VERBOSE;
      }
      virtual ~HybridCallbackProxy() {
        LOG_VERBOSE;
      }

    public:
      SEXP get_subset(const SymbolString& name) const {
        return real->get_subset(name);
      }

    private:
      const IHybridCallback* real;
    };

  public:
    GroupedHybridEval(const Subsets& subsets_) :
      indices(NULL),
      subsets(subsets_),
      proxy(new HybridCallbackProxy(this))
    {
      LOG_VERBOSE;
    }

    const Index& get_indices() const {
      return *indices;
    }

  public: // IHybridCallback
    SEXP get_subset(const SymbolString& name) const {
      return subsets.get(name, get_indices());
    }

  public:
    void set_indices(const Index& indices_) {
      indices = &indices_;
    }

  private:
    const Index* indices;
    const LazySplitSubsets<Data>& subsets;

    boost::shared_ptr<IHybridCallback> proxy;

  };

public:
  DataMask_bindings_active(SEXP parent_env, Subsets& subsets_) :
    subsets(subsets_),
    callback(new GroupedHybridEval(subsets))
  {
    CharacterVector names = subsets.get_variable_names().get_vector();

    XPtr<const HybridCallbackWeakProxy> p(new HybridCallbackWeakProxy(callback));
    List payload = List::create(p);

    // Environment::new_child() performs an R callback, creating the environment
    // in R should be slightly faster
    mask_active = bindrcpp::create_env_string_wrapped(
                    names, &DataMask_bindings_active::hybrid_get_callback,
                    payload, parent_env
                  );
  }

  inline operator SEXP() {
    return mask_active;
  }

  void update(const Index& indices) {
    subsets.clear();
    callback->set_indices(indices);
  }

private:
  Environment mask_active;
  Subsets& subsets ;
  boost::shared_ptr<GroupedHybridEval> callback;

  static SEXP hybrid_get_callback(const String& name, List payload) {
    XPtr<const HybridCallbackWeakProxy> callback_ = payload[0];
    return callback_->get_subset(SymbolString(name));
  }

};

}

#endif
