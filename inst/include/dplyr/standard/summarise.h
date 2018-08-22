#ifndef dplyr_standard_summarise_h
#define dplyr_standard_summarise_h

#include <tools/Quosure.h>

namespace dplyr {
namespace standard {

template <typename SlicedTibble>
SEXP summarise(const NamedQuosure& quosure, const SlicedTibble& data, LazySplitSubsets<SlicedTibble>& subsets) {
  DataMask<SlicedTibble> data_mask(subsets, quosure.env());
  return GroupedCallReducer<SlicedTibble>(quosure.expr(), quosure.name(), data_mask).process(data);
}

}
}


#endif
