#ifndef dplyr_hybrid_sum_h
#define dplyr_hybrid_sum_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Dispatch.h>

namespace dplyr {
namespace hybrid {

namespace internal {

template <typename STORAGE, typename Index, bool NA_RM>
struct SumImpl {

  static STORAGE process(STORAGE* data_ptr, const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      STORAGE value = data_ptr[indices[i]];

      if (Rcpp::traits::is_na<INTSXP>(value)) {
        if (NA_RM) {
          continue;
        }

        return Rcpp::traits::get_na<INTSXP>();
      }

      res += value;
    }

    if (res > INT_MAX || res <= INT_MIN) {
      warning("integer overflow - use sum(as.numeric(.))");
      return Rcpp::traits::get_na<INTSXP>();
    }

    return (STORAGE)res;
  }

};

template <typename Index, bool NA_RM>
struct SumImpl<double, Index, NA_RM> {

  static double process(double* data_ptr, const Index& indices) {
    long double res = 0;
    int n = indices.size();
    for (int i = 0; i < n; i++) {
      double value = data_ptr[indices[i]];

      if (NA_RM && Rcpp::traits::is_na<REALSXP>(value)) {
        continue;
      }

      if (!NA_RM && Rcpp::traits::is_na<REALSXP>(value)) {
        return NA_REAL;
      }

      res += value;
    }

    return (double)res;
  }
};

// General case (for INTSXP and LGLSXP)
template <int RTYPE, bool NA_RM, typename Data>
class SumTemplate : public HybridVectorScalarResult < RTYPE == LGLSXP ? INTSXP : RTYPE, Data, SumTemplate<RTYPE, NA_RM, Data> >  {
public :
  static const int rtype = RTYPE == LGLSXP ? INTSXP : RTYPE;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  typedef HybridVectorScalarResult<rtype, Data, SumTemplate> Parent ;
  typedef typename Data::slicing_index Index;

  SumTemplate(const Data& data_, Column column_) :
    Parent(data_),
    data_ptr(Rcpp::internal::r_vector_start<RTYPE>(column_.data)),
    is_summary(column_.is_summary)
  {}

  STORAGE process(const Index& indices) const {
    // already summarised, e.g. when summarise( x = ..., y = sum(x))
    if (is_summary) return data_ptr[indices.group()];

    return SumImpl<STORAGE, Index, NA_RM>::process(data_ptr, indices);
  }

private:

  STORAGE* data_ptr;
  bool is_summary;
};

template <typename Data>
class SumDispatch {
public:
  typedef typename Data::slicing_index Index;

  SumDispatch(const Data& data_, Column variable_, bool narm_):
    data(data_),
    variable(variable_),
    narm(narm_)
  {}

  SEXP summarise() const {
    return operate(Summary());
  }

  SEXP window() const {
    return operate(Window());
  }

private:
  const Data& data;
  Column variable;
  bool narm;

  template <typename Operation>
  SEXP operate(const Operation& op) const {
    // dispatch to the method below based on na.rm
    if (narm) {
      return operate_narm<Operation, true>(op);
    } else {
      return operate_narm<Operation, false>(op);
    }
  }

  template <typename Operation, bool NARM>
  SEXP operate_narm(const Operation& op) const {
    // try to dispatch to the right class
    switch (TYPEOF(variable.data)) {
    case INTSXP:
      return op(SumTemplate<INTSXP, NARM, Data>(data, variable));
    case REALSXP:
      return op(SumTemplate<REALSXP, NARM, Data>(data, variable));
    case LGLSXP:
      return op(SumTemplate<LGLSXP, NARM, Data>(data, variable));
    }

    // give up, effectively let R evaluate the call
    return R_UnboundValue;
  }

};


} // namespace internal

template <typename Data>
internal::SumDispatch<Data> sum_(const Data& data, Column variable, bool narm) {
  return internal::SumDispatch<Data>(data, variable, narm);
}

}
}


#endif
