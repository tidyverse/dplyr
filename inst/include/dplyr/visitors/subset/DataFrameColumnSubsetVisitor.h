#ifndef dplyr_DataFrameColumnSubsetVisitors_H
#define dplyr_DataFrameColumnSubsetVisitors_H

#include <dplyr/visitors/subset/SubsetVectorVisitor.h>
#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>

namespace dplyr {

class DataFrameColumnSubsetVisitor : public SubsetVectorVisitor {
public:
  DataFrameColumnSubsetVisitor(const DataFrame& data_) : data(data_), visitors(data) {}

  inline SEXP subset(const Rcpp::IntegerVector& index) const {
    return visitors.subset(index, get_class(data));
  }

  inline SEXP subset(const std::vector<int>& index) const {
    return visitors.subset(index, get_class(data));
  }

  inline SEXP subset(const SlicingIndex& index) const {
    return visitors.subset(index, get_class(data));
  }

  inline int size() const {
    return visitors.nrows();
  }

private:
  DataFrame data;
  DataFrameSubsetVisitors visitors;
};

}

#endif
