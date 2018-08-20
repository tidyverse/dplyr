#ifndef dplyr_GroupedSubsetBase_H
#define dplyr_GroupedSubsetBase_H

#include <tools/SlicingIndex.h>

namespace dplyr {

template <typename Index>
class Subset {
public:
  Subset() {};
  virtual ~Subset() {};
  virtual SEXP get(const Index& indices) = 0;
  virtual SEXP get_variable() const = 0;
  virtual bool is_summary() const = 0;
};

}

#endif //dplyr_GroupedSubsetBase_H
