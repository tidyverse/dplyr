#ifndef dplyr_DataFrameVisitors_H
#define dplyr_DataFrameVisitors_H

#include <tools/pointer_vector.h>

#include <dplyr/visitor_set/VisitorSetMixin.h>

#include <dplyr/VectorVisitor.h>
#include <tools/SymbolVector.h>

namespace dplyr {

class DataFrameVisitors :
  public VisitorSetEqual<DataFrameVisitors>,
  public VisitorSetHash<DataFrameVisitors>,
  public VisitorSetLess<DataFrameVisitors>,
  public VisitorSetGreater<DataFrameVisitors> {

private:

  const Rcpp::DataFrame& data;
  pointer_vector<VectorVisitor> visitors;
  SymbolVector visitor_names;

public:
  typedef VectorVisitor visitor_type;

  DataFrameVisitors(const DataFrame& data_);

  DataFrameVisitors(const DataFrame& data_, const SymbolVector& names);

  DataFrameVisitors(const DataFrame& data_, const IntegerVector& indices);

  inline int size() const {
    return visitors.size();
  }
  inline VectorVisitor* get(int k) const {
    return visitors[k];
  }

  const SymbolString name(int k) const {
    return visitor_names[k];
  }

  inline int nrows() const {
    return data.nrows();
  }

};

} // namespace dplyr


#endif
