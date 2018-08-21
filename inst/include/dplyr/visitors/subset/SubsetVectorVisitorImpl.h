#ifndef dplyr_SubsetVectorVisitor_Impl_H
#define dplyr_SubsetVectorVisitor_Impl_H

#include <tools/collapse.h>
#include <tools/utils.h>
#include <tools/default_value.h>

#include <dplyr/visitors/vector/VectorVisitorImpl.h>
#include <dplyr/visitors/subset/SubsetVectorVisitor.h>

namespace dplyr {

/**
 * Implementations
 */
template <int RTYPE>
class SubsetVectorVisitorImpl : public SubsetVectorVisitor {
public:
  typedef Rcpp::Vector<RTYPE> VECTOR;

  /**
   * The type of data : int, double, SEXP, Rcomplex
   */
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  SubsetVectorVisitorImpl(const VECTOR& vec_) : vec(vec_) {}

  inline SEXP subset(const Rcpp::IntegerVector& index) const {
    return subset_int_index(index);
  }

  inline SEXP subset(const std::vector<int>& index) const {
    return subset_int_index(index);
  }

  inline SEXP subset(const SlicingIndex& index) const {
    return subset_int_index(index);
  }

  inline SEXP subset(EmptySubset) const {
    VECTOR out(0);
    copy_most_attributes(out, vec);
    return out;
  }

  inline int size() const {
    return vec.size();
  }

protected:
  VECTOR vec;

  template <typename Container>
  inline SEXP subset_int_index(const Container& index) const {
    int n = output_size(index);
    VECTOR out(no_init(n));
    for (int i = 0; i < n; i++) {
      out[i] = index[i] < 0 ? default_value<RTYPE>() : (STORAGE)vec[ index[i] ];
    }
    copy_most_attributes(out, vec);
    return out;
  }

};

class SubsetFactorVisitor : public SubsetVectorVisitorImpl<INTSXP> {
public:
  typedef SubsetVectorVisitorImpl<INTSXP> Parent;

  SubsetFactorVisitor(const IntegerVector& vec_) : Parent(vec_) {
    levels = get_levels(vec);
    levels_ptr = Rcpp::internal::r_vector_start<STRSXP>(levels);
  }

  inline SEXP subset(const Rcpp::IntegerVector& index) const {
    return promote(Parent::subset(index));
  }

  inline SEXP subset(const SlicingIndex& index) const {
    return promote(Parent::subset(index));
  }

  inline SEXP subset(const std::vector<int>& index) const {
    return promote(Parent::subset(index));
  }

  inline SEXP subset(EmptySubset empty) const {
    return promote(Parent::subset(empty));
  }

private:
  inline bool same_levels(SubsetFactorVisitor* other, std::stringstream& ss, const SymbolString& name) const {
    CharacterVector levels_other = other->levels;

    if (!character_vector_equal(levels, levels_other)) {
      ss << "Factor levels not equal for column `" << name.get_utf8_cstring() << "`";
      return false;
    }
    return true;
  }

  inline SEXP promote(IntegerVector x) const {
    copy_most_attributes(x, vec);
    return x;
  }

  CharacterVector levels;
  SEXP* levels_ptr;

};

class DateSubsetVectorVisitor : public SubsetVectorVisitor {
public:

  DateSubsetVectorVisitor(SEXP data) : impl(0) {
    if (TYPEOF(data) == INTSXP) {
      impl  = new SubsetVectorVisitorImpl<INTSXP>(data);
    } else if (TYPEOF(data) == REALSXP) {
      impl = new SubsetVectorVisitorImpl<REALSXP>(data);
    } else {
      stop("Unreachable");
    }
  }

  ~DateSubsetVectorVisitor() {
    delete impl;
  }

  virtual SEXP subset(const Rcpp::IntegerVector& index) const {
    return impl->subset(index);
  }

  virtual SEXP subset(const SlicingIndex& index) const {
    return impl->subset(index);
  }

  virtual SEXP subset(const std::vector<int>& index) const {
    return impl->subset(index);
  }

  virtual SEXP subset(EmptySubset index) const {
    return impl->subset(index);
  }

  virtual int size() const {
    return impl->size();
  }

private:
  SubsetVectorVisitor* impl;
  DateSubsetVectorVisitor(const DateSubsetVectorVisitor&);

};

}

#endif
