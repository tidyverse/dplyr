#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

#include <dplyr/visitors/subset/DataFrameSubsetVisitors.h>
#include <dplyr/subset/Subset.h>
#include <tools/utils.h>

namespace dplyr {

template <int RTYPE, typename Index>
class SubsetTemplate : public Subset<Index> {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  SubsetTemplate(SEXP x) :
    object(x), start(Rcpp::internal::r_vector_start<RTYPE>(object)) {}

  virtual SEXP get(const Index& indices) {
    int n = indices.size();
    Vector<RTYPE> data(no_init(n));
    copy_most_attributes(data, object);
    for (int i = 0; i < n; i++) {
      data[i] = start[indices[i]];
    }
    return data;
  }
  virtual SEXP get_variable() const {
    return object;
  }
  virtual bool is_summary() const {
    return false;
  }

private:
  SEXP object;
  STORAGE* start;
};

template <typename Index>
class DataFrameSubset : public Subset<Index> {
public:
  DataFrameSubset(SEXP x) : data(x), visitors(data) {}

  virtual SEXP get(const Index& indices) {
    return visitors.subset(indices, get_class(data));
  }

  virtual SEXP get_variable() const {
    return data;
  }

  virtual bool is_summary() const {
    return false;
  }

private:
  DataFrame data;
  DataFrameSubsetVisitors visitors;
};

template <>
class SubsetTemplate<VECSXP, RowwiseSlicingIndex> : public Subset<RowwiseSlicingIndex> {
public:
  SubsetTemplate(SEXP x) :
    object(x), start(Rcpp::internal::r_vector_start<VECSXP>(object))
  {}

  virtual SEXP get(const RowwiseSlicingIndex& indices) {
    return start[ indices.group() ];
  }
  virtual SEXP get_variable() const {
    return object;
  }
  virtual bool is_summary() const {
    return false;
  }

private:
  SEXP object;
  SEXP* start;
};

template <typename Index>
inline Subset<Index>* get_subset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:
    return new SubsetTemplate<INTSXP, Index>(x);
  case REALSXP:
    return new SubsetTemplate<REALSXP, Index>(x);
  case LGLSXP:
    return new SubsetTemplate<LGLSXP, Index>(x);
  case STRSXP:
    return new SubsetTemplate<STRSXP, Index>(x);
  case VECSXP:
    if (Rf_inherits(x, "data.frame"))
      return new DataFrameSubset<Index>(x);
    if (Rf_inherits(x, "POSIXlt")) {
      stop("POSIXlt not supported");
    }
    return new SubsetTemplate<VECSXP, Index>(x);
  case CPLXSXP:
    return new SubsetTemplate<CPLXSXP, Index>(x);
  case RAWSXP:
    return new SubsetTemplate<RAWSXP, Index>(x);
  default:
    break;
  }
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(x)));
}

}

#endif
