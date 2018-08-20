#ifndef dplyr_RowwiseSubset_H
#define dplyr_RowwiseSubset_H

#include <tools/utils.h>

#include <dplyr/checks.h>

#include <dplyr/subset/GroupedSubsetBase.h>

namespace dplyr {

template <int RTYPE>
class RowwiseSubsetTemplate : public Subset<RowwiseSlicingIndex> {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
  RowwiseSubsetTemplate(SEXP x) :
    object(x), start(Rcpp::internal::r_vector_start<RTYPE>(object))
  {}

  ~RowwiseSubsetTemplate() {}

  virtual SEXP get(const RowwiseSlicingIndex& indices) {
    Vector<RTYPE> output(1, start[indices.group()]);
    copy_most_attributes(output, object);
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

template <>
class RowwiseSubsetTemplate<VECSXP> : public Subset<RowwiseSlicingIndex> {
public:
  RowwiseSubsetTemplate(SEXP x) :
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


inline Subset<RowwiseSlicingIndex>* rowwise_subset(SEXP x) {
  switch (check_supported_type(x)) {
  case DPLYR_INTSXP:
    return new RowwiseSubsetTemplate<INTSXP>(x);
  case DPLYR_REALSXP:
    return new RowwiseSubsetTemplate<REALSXP>(x);
  case DPLYR_LGLSXP:
    return new RowwiseSubsetTemplate<LGLSXP>(x);
  case DPLYR_STRSXP:
    return new RowwiseSubsetTemplate<STRSXP>(x);
  case DPLYR_CPLXSXP:
    return new RowwiseSubsetTemplate<CPLXSXP>(x);
  case DPLYR_VECSXP:
    return new RowwiseSubsetTemplate<VECSXP>(x);
  case DPLYR_RAWSXP:
    return new RowwiseSubsetTemplate<RAWSXP>(x);
  }

  stop("Unreachable");
  return 0;
}

}

#endif
