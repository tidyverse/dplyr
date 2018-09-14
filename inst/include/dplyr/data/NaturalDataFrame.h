#ifndef dplyr_tools_NaturalDataFrame_H
#define dplyr_tools_NaturalDataFrame_H

#include <dplyr/registration.h>
#include <tools/SlicingIndex.h>

#include <tools/SymbolVector.h>
#include <tools/bad.h>

namespace dplyr {

class NaturalDataFrame;

class NaturalDataFrameIndexIterator {
public:
  NaturalDataFrameIndexIterator(int n_): n(n_) {}

  NaturalDataFrameIndexIterator& operator++() {
    return *this;
  }

  NaturalSlicingIndex operator*() const {
    return NaturalSlicingIndex(n);
  }

private:
  int n;
};

class NaturalDataFrame {
public:
  typedef NaturalDataFrameIndexIterator group_iterator;
  typedef NaturalSlicingIndex slicing_index;

  NaturalDataFrame(SEXP x):
    data_(x)
  {}
  NaturalDataFrame(SEXP x, const NaturalDataFrame& /* model */):
    data_(x)
  {}

  NaturalDataFrameIndexIterator group_begin() const {
    return NaturalDataFrameIndexIterator(nrows());
  }

  SymbolString symbol(int i) const {
    return SymbolString() ;
  }

  DataFrame& data() {
    return data_;
  }
  const DataFrame& data() const {
    return data_;
  }

  inline int ngroups() const {
    return 1;
  }

  inline int nvars() const {
    return 0;
  }

  inline int nrows() const {
    return data_.nrows();
  }

  inline SEXP label(int i) const {
    return R_NilValue ;
  }

  inline bool has_group(const SymbolString& g) const {
    return false ;
  }

  inline int size() const {
    return data_.size() ;
  }
  inline SEXP operator[](int i) {
    return data_[i];
  }

  inline SymbolVector get_vars() const {
    return SymbolVector();
  }

  static inline CharacterVector classes() {
    return Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
  }

private:

  DataFrame data_;

};

}

#endif
