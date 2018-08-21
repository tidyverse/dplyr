#ifndef dplyr_SubsetVectorVisitor_H
#define dplyr_SubsetVectorVisitor_H

#include <tools/SlicingIndex.h>
#include <tools/SymbolString.h>

namespace dplyr {

template <typename Container>
inline int output_size(const Container& container) {
  return container.size();
}

/**
 * Subset Vector visitor base class, defines the interface
 */
class SubsetVectorVisitor {
public:
  virtual ~SubsetVectorVisitor() {}

  /** creates a new vector, of the same type as the visited vector, by
   *  copying elements at the given indices
   */
  virtual SEXP subset(const Rcpp::IntegerVector& index) const = 0;

  virtual SEXP subset(const std::vector<int>&) const = 0;

  virtual SEXP subset(const SlicingIndex&) const = 0;

  virtual int size() const = 0;

};

inline SubsetVectorVisitor* subset_visitor(SEXP vec, const SymbolString& name);

} // namespace dplyr


#endif
