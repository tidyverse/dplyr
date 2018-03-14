#ifndef dplyr_DataFrameSubsetVisitors_H
#define dplyr_DataFrameSubsetVisitors_H

#include <dplyr/subset_visitor.h>

#include <tools/pointer_vector.h>
#include <tools/SymbolVector.h>

namespace dplyr {

class DataFrameSubsetVisitors {

private:

  const DataFrame& data;
  pointer_vector<SubsetVectorVisitor> visitors;
  SymbolVector visitor_names;

public:
  typedef SubsetVectorVisitor visitor_type;

  DataFrameSubsetVisitors(const DataFrame& data_);

  DataFrameSubsetVisitors(const DataFrame& data_, const SymbolVector& names);

  DataFrameSubsetVisitors(const DataFrame& data_, const IntegerVector& indices);

  template <typename Container>
  DataFrame subset(const Container& index, const CharacterVector& classes) const {
    List out = get_subsets(index);
    structure(out, output_size(index), classes);
    return out;
  }

  int size() const;
  SubsetVectorVisitor* get(int k) const;
  const SymbolString name(int k) const;
  int nrows() const;

private:

  template <typename Container>
  List get_subsets(const Container& index) const {
    const int nvisitors = visitors.size();
    List out(nvisitors);
    for (int k = 0; k < nvisitors; k++) {
      out[k] = get_subset(index, k);
    }
    return out;
  }

  template <typename Container>
  SEXP get_subset(const Container& index, int k) const {
    return get(k)->subset(index);
  }

  void structure(List& x, int nrows, CharacterVector classes) const;

};

template <>
DataFrame DataFrameSubsetVisitors::subset(const LogicalVector& index, const CharacterVector& classes) const;

template <typename Index>
DataFrame subset(DataFrame df, const Index& indices, const SymbolVector& columns, const CharacterVector& classes) {
  return DataFrameSubsetVisitors(df, columns).subset(indices, classes);
}

template <typename Index>
DataFrame subset(DataFrame df, const Index& indices, CharacterVector classes) {
  return DataFrameSubsetVisitors(df).subset(indices, classes);
}

} // namespace dplyr

#include <dplyr/subset_visitor_impl.h>

#endif
