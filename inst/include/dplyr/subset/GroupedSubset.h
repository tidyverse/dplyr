#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

#include <dplyr/DataFrameSubsetVisitors.h>
#include <dplyr/SummarisedVariable.h>
#include <dplyr/subset/GroupedSubsetBase.h>

namespace dplyr {

template <int RTYPE>
class NaturalSubsetTemplate : public GroupedSubset {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  NaturalSubsetTemplate(SEXP x) :
    object(x), start(Rcpp::internal::r_vector_start<RTYPE>(object)) {}

  virtual SEXP get(const SlicingIndex& indices) {
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

inline NaturalSubset* natural_subset(SEXP x) {
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
class GroupedSubsetTemplate : public GroupedSubset {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  GroupedSubsetTemplate(SEXP x) :
    object(x),
    start(Rcpp::internal::r_vector_start<RTYPE>(object))
  {}

  virtual SEXP get(const SlicingIndex& indices) {
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

class DataFrameGroupedSubset : public GroupedSubset {
public:
  DataFrameGroupedSubset(SEXP x) : data(x), visitors(data) {}

  virtual SEXP get(const SlicingIndex& indices) {
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

inline GroupedSubset* grouped_subset(SEXP x) {
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


template <int RTYPE>
class SummarisedSubsetTemplate : public GroupedSubset {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  SummarisedSubsetTemplate(SummarisedVariable x) :
    object(x), output(1)
  {
    copy_most_attributes(output, object);
  }

  virtual SEXP get(const SlicingIndex& indices) {
    output[0] = object[indices.group()];
    return output;
  }
  virtual SEXP get_variable() const {
    return object;
  }
  virtual bool is_summary() const {
    return true;
  }

private:
  Rcpp::Vector<RTYPE> object;
  Rcpp::Vector<RTYPE> output;
};

template <>
inline SEXP SummarisedSubsetTemplate<VECSXP>::get(const SlicingIndex& indices) {
  return List::create(object[indices.group()]);
}

inline GroupedSubset* summarised_subset(SummarisedVariable x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return new SummarisedSubsetTemplate<LGLSXP>(x);
  case INTSXP:
    return new SummarisedSubsetTemplate<INTSXP>(x);
  case REALSXP:
    return new SummarisedSubsetTemplate<REALSXP>(x);
  case STRSXP:
    return new SummarisedSubsetTemplate<STRSXP>(x);
  case VECSXP:
    return new SummarisedSubsetTemplate<VECSXP>(x);
  case CPLXSXP:
    return new SummarisedSubsetTemplate<CPLXSXP>(x);
  case RAWSXP:
    return new SummarisedSubsetTemplate<RAWSXP>(x);
  default:
    break;
  }
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(x)));
}
}

#endif
