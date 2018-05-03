#ifndef dplyr_Result_H
#define dplyr_Result_H

#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/GroupedDataFrame.h>
#include <dplyr/NaturalDataFrame.h>
#include <tools/SlicingIndex.h>

namespace dplyr {

class Result {
public:
  Result() {}

  virtual ~Result() {};

  virtual SEXP process(const RowwiseDataFrame& gdf) = 0;

  virtual SEXP process(const GroupedDataFrame& gdf) = 0;

  virtual SEXP process(const SlicingIndex&) {
    return R_NilValue;
  }

  virtual SEXP process(const NaturalDataFrame& gdf) {
    return process(NaturalSlicingIndex(gdf.nrows()));
  }

};

} // namespace dplyr

#endif
