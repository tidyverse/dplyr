#ifndef dplyr_summarised_subset_H
#define dplyr_summarised_subset_H

#include <dplyr/subset/Subset.h>

namespace dplyr {

template <int RTYPE, typename Index>
class SummarisedSubsetTemplate : public Subset<Index> {
public:
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  SummarisedSubsetTemplate(SEXP x) :
    object(x), output(1)
  {
    copy_most_attributes(output, object);
  }

  virtual SEXP get(const Index& indices) {
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

template <typename Index>
class SummarisedSubsetTemplate<VECSXP, Index> : public Subset<Index> {
public:
  SummarisedSubsetTemplate(SEXP x) :
    object(x), output(1)
  {
    copy_most_attributes(output, object);
  }

  virtual SEXP get(const Index& indices) {
    return Rcpp::List::create(object[indices.group()]);
  }
  virtual SEXP get_variable() const {
    return object;
  }
  virtual bool is_summary() const {
    return true;
  }

private:
  Rcpp::List object;
  Rcpp::List output;
};

template <typename Index>
inline Subset<Index>* summarised_subset(SEXP x) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return new SummarisedSubsetTemplate<LGLSXP, Index>(x);
  case INTSXP:
    return new SummarisedSubsetTemplate<INTSXP, Index>(x);
  case REALSXP:
    return new SummarisedSubsetTemplate<REALSXP, Index>(x);
  case STRSXP:
    return new SummarisedSubsetTemplate<STRSXP, Index>(x);
  case VECSXP:
    return new SummarisedSubsetTemplate<VECSXP, Index>(x);
  case CPLXSXP:
    return new SummarisedSubsetTemplate<CPLXSXP, Index>(x);
  case RAWSXP:
    return new SummarisedSubsetTemplate<RAWSXP, Index>(x);
  default:
    break;
  }
  Rcpp::stop("is of unsupported type %s", Rf_type2char(TYPEOF(x)));
}
}

#endif
