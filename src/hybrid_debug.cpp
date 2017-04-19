#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Result.h>

using namespace Rcpp;
using namespace dplyr;


class VerifyHybrid : public Result {
public:
  explicit VerifyHybrid(SEXP x_) : x(x_) {}

public:
  SEXP process(const RowwiseDataFrame&) {
    return x;
  }

  SEXP process(const GroupedDataFrame&) {
    return x;
  }

  SEXP process(const FullDataFrame&) {
    return x;
  }

  SEXP process(const SlicingIndex&) {
    return x;
  }

private:
  RObject x;
};

Result* verify_hybrid_prototype(SEXP call, const ILazySubsets&, int nargs) {
  // if not exactly one arg, let R handle it
  if (nargs != 1)
    return 0;

  // if it isn't a constant, let R handle it
  SEXP arg = CADR(call);
  if (TYPEOF(arg) == SYMSXP || TYPEOF(arg) == LANGSXP)
    return 0;

  return new VerifyHybrid(arg);
}

class VerifyNotHybrid : public Result {
public:
  explicit VerifyNotHybrid(SEXP x_) : x(x_) {}

public:
  SEXP process(const RowwiseDataFrame&) {
    stop("In hybrid evaluation");
  }

  SEXP process(const GroupedDataFrame&) {
    stop("In hybrid evaluation");
  }

  SEXP process(const FullDataFrame&) {
    stop("In hybrid evaluation");
  }

  SEXP process(const SlicingIndex&) {
    stop("In hybrid evaluation");
  }

private:
  RObject x;
};

Result* verify_not_hybrid_prototype(SEXP call, const ILazySubsets&, int nargs) {
  // if not exactly one arg, let R handle it
  if (nargs != 1)
    return 0;

  // if it isn't a constant, let R handle it
  SEXP arg = CADR(call);
  if (TYPEOF(arg) == SYMSXP || TYPEOF(arg) == LANGSXP)
    return 0;

  return new VerifyNotHybrid(arg);
}

void install_debug_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("verify_hybrid") ] = verify_hybrid_prototype;
  handlers[ Rf_install("verify_not_hybrid") ] = verify_not_hybrid_prototype;
}
