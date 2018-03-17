#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

#include <boost/scoped_ptr.hpp>

#include <tools/Call.h>
#include <tools/utils.h>

#include <dplyr/Result/Result.h>

#include <bindrcpp.h>

namespace dplyr {

SEXP dplyr_object(const char* name);


class IHybridCallback {
protected:
  virtual ~IHybridCallback();

public:
  virtual SEXP get_subset(const SymbolString& name) const = 0;
};


class GroupedHybridEnv {
public:
  GroupedHybridEnv(const CharacterVector& names_, const Environment& env_, const IHybridCallback* callback_);

  ~GroupedHybridEnv();

public:
  const Environment& get_overscope() const;

private:
  void provide_overscope() const;

  static SEXP hybrid_get_callback(const String& name, bindrcpp::PAYLOAD payload);

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


class GroupedHybridEval : public IHybridCallback {
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
  const GroupedHybridEnv hybrid_env;
  const GroupedHybridCall hybrid_call;
};


} // namespace dplyr

#endif
