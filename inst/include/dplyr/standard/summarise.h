#ifndef dplyr_standard_summarise_h
#define dplyr_standard_summarise_h

#include <tools/Quosure.h>

namespace dplyr {
namespace standard {

// template <typename SlicedTibble>
// SEXP summarise(const NamedQuosure& quosure, const SlicedTibble& data, LazySplitSubsets<SlicedTibble>& subsets) {
//   return GroupedCallReducer<SlicedTibble>(quosure.expr(), quosure.name(), subsets).process(data);
// }

}
}


#endif
