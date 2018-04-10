#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>

#include <dplyr/Result/Result.h>

using namespace Rcpp;
using namespace dplyr;


class GroupHybrid : public Result {
public:
  SEXP process(const RowwiseDataFrame&) {
    Rprintf("GroupHybrid::RowwiseDataFrame()\n") ;
    return R_NilValue ;
  }

  SEXP process(const GroupedDataFrame&) {
    Rprintf("GroupHybrid::GroupedDataFrame()\n") ;
    return R_NilValue ;
  }

  SEXP process(const SlicingIndex& i) {
    return IntegerVector(i.size(), clean_group(i.group()));
  }

private:
  int clean_group(int i) {
    return i < 0 ? 1 : (i + 1);
  }
};

Result* group_prototype(SEXP call, const ILazySubsets&, int nargs) {
  if (nargs != 0)
    return 0;

  return new GroupHybrid();
}

void install_group_handlers(HybridHandlerMap& handlers) {
  Environment ns_dplyr = Environment::namespace_env("dplyr");
  handlers[ Rf_install("group_indices") ] = HybridHandler(group_prototype, HybridHandler::DPLYR, ns_dplyr["group_indices"]);
}
