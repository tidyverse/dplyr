#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

#include <dplyr/DataFrameSubsetVisitors.h>
#include <dplyr/SummarisedVariable.h>
#include <dplyr/subset/GroupedSubsetBase.h>

namespace dplyr {

template <int RTYPE>
class NaturalSubsetTemplate : public Subset<NaturalSlicingIndex> {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  NaturalSubsetTemplate(SEXP x) :
    object(x), start(Rcpp::internal::r_vector_start<RTYPE>(object)) {}

  virtual SEXP get(const NaturalSlicingIndex& indices) {
    int n = indices.size();
    Vector<RTYPE> data = no_init(n);
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

inline Subset<NaturalSlicingIndex>* natural_subset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:
    return new NaturalSubsetTemplate<INTSXP>(x);
  case REALSXP:
    return new NaturalSubsetTemplate<REALSXP>(x);
  case LGLSXP:
    return new NaturalSubsetTemplate<LGLSXP>(x);
  case STRSXP:
    return new NaturalSubsetTemplate<STRSXP>(x);
  case VECSXP:
    // if (Rf_inherits(x, "data.frame"))
    //   return new DataFrameGroupedSubset(x);
    if (Rf_inherits(x, "POSIXlt")) {
      stop("POSIXlt not supported");
    }
    return new NaturalSubsetTemplate<VECSXP>(x);
  case CPLXSXP:
    return new NaturalSubsetTemplate<CPLXSXP>(x);
  case RAWSXP:
    return new NaturalSubsetTemplate<RAWSXP>(x);
  default:
    break;
  }
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(x)));
}

template <int RTYPE>
class GroupedSubsetTemplate : public Subset<GroupedSlicingIndex> {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  GroupedSubsetTemplate(SEXP x) :
    object(x),
    start(Rcpp::internal::r_vector_start<RTYPE>(object))
  {}

  virtual SEXP get(const GroupedSlicingIndex& indices) {
    int n = indices.size();
    Vector<RTYPE> output = no_init(n);
    copy_most_attributes(output, object);
    for (int i = 0; i < n; i++) {
      output[i] = start[indices[i]];
    }
    return output;
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

class DataFrameGroupedSubset : public Subset<GroupedSlicingIndex> {
public:
  DataFrameGroupedSubset(SEXP x) : data(x), visitors(data) {}

  virtual SEXP get(const GroupedSlicingIndex& indices) {
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

inline Subset<GroupedSlicingIndex>* grouped_subset(SEXP x) {
  switch (TYPEOF(x)) {
  case INTSXP:
    return new GroupedSubsetTemplate<INTSXP>(x);
  case REALSXP:
    return new GroupedSubsetTemplate<REALSXP>(x);
  case LGLSXP:
    return new GroupedSubsetTemplate<LGLSXP>(x);
  case STRSXP:
    return new GroupedSubsetTemplate<STRSXP>(x);
  case VECSXP:
    if (Rf_inherits(x, "data.frame"))
      return new DataFrameGroupedSubset(x);
    if (Rf_inherits(x, "POSIXlt")) {
      stop("POSIXlt not supported");
    }
    return new GroupedSubsetTemplate<VECSXP>(x);
  case CPLXSXP:
    return new GroupedSubsetTemplate<CPLXSXP>(x);
  case RAWSXP:
    return new GroupedSubsetTemplate<RAWSXP>(x);
  default:
    break;
  }
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(x)));
}

}

#endif
