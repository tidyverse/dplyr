#ifndef dplyr_hybrid_first_last_h
#define dplyr_hybrid_first_last_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/default_value.h>

namespace dplyr {
namespace hybrid {

namespace internal{

template <int RTYPE, typename Data>
class First1 : public HybridVectorScalarResult<RTYPE, Data, First1<RTYPE, Data> > {
public:
  typedef HybridVectorScalarResult<RTYPE, Data, First1> Parent ;
  typedef typename Data::slicing_index Index;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  First1(const Data& data, SEXP column_):
    Parent(data),
    column(column_),
    def(default_value<RTYPE>())
  {}

  First1(const Data& data, SEXP column_, STORAGE def_):
    Parent(data),
    column(column_),
    def(def_)
  {}

  inline STORAGE process(const Index& indices) const {
    return indices.size() ? (STORAGE)column[0] : def ;
  }

private:
  Rcpp::Vector<RTYPE> column;
  STORAGE def;
};

template <int RTYPE, typename Data>
class Last1 : public HybridVectorScalarResult<RTYPE, Data, Last1<RTYPE, Data> > {
public:
  typedef HybridVectorScalarResult<RTYPE, Data, Last1> Parent ;
  typedef typename Data::slicing_index Index;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  Last1(const Data& data, SEXP column_):
    Parent(data),
    column(column_),
    def(default_value<RTYPE>())
  {}

  Last1(const Data& data, SEXP column_, STORAGE def_):
    Parent(data),
    column(column_),
    def(def_)
  {}


  inline STORAGE process(const Index& indices) const {
    return indices.size() ? (STORAGE)column[indices.size()-1] : def ;
  }

private:
  Rcpp::Vector<RTYPE> column;
  STORAGE def;
};
}

template <typename Data, typename Operation, template <int, typename> class Impl>
SEXP firstlast_1(const Data& data, SEXP x, const Operation& op) {
  switch(TYPEOF(x)){
  case LGLSXP: return op(internal::First1<LGLSXP, Data>(data, x));
  case RAWSXP: return op(internal::First1<RAWSXP, Data>(data, x));
  case INTSXP: return op(internal::First1<INTSXP, Data>(data, x));
  case REALSXP: return op(internal::First1<REALSXP, Data>(data, x));
  case CPLXSXP: return op(internal::First1<CPLXSXP, Data>(data, x));
  case STRSXP: return op(internal::First1<STRSXP, Data>(data, x));
  case VECSXP: return op(internal::First1<VECSXP, Data>(data, x));
  default: break;
  }
  return R_UnboundValue;
}

// first( <column> )
template <typename Data, typename Operation>
SEXP first1_(const Data& data, SEXP x, const Operation& op) {
  return firstlast_1<Data, Operation, internal::First1>(data, x, op);
}

// last( <column> )
template <typename Data, typename Operation>
SEXP last1_(const Data& data, SEXP x, const Operation& op) {
  return firstlast_1<Data, Operation, internal::Last1>(data, x, op);
}

template <typename Data, typename Operation, template <int, typename> class Impl>
SEXP firstlast_2_default(const Data& data, SEXP x, SEXP def, const Operation& op) {
  if (TYPEOF(def) != TYPEOF(def) || Rf_length(def) != 1) return R_UnboundValue;

  switch(TYPEOF(x)){
  case LGLSXP: return op(Impl<LGLSXP, Data>(data, x, Rcpp::Vector<LGLSXP>(def)[0]));
  case RAWSXP: return op(Impl<RAWSXP, Data>(data, x, Rcpp::Vector<RAWSXP>(def)[0]));
  case INTSXP: return op(Impl<INTSXP, Data>(data, x, Rcpp::Vector<INTSXP>(def)[0]));
  case REALSXP: return op(Impl<REALSXP, Data>(data, x, Rcpp::Vector<REALSXP>(def)[0]));
  case CPLXSXP: return op(Impl<CPLXSXP, Data>(data, x, Rcpp::Vector<CPLXSXP>(def)[0]));
  case STRSXP: return op(Impl<STRSXP, Data>(data, x, Rcpp::Vector<STRSXP>(def)[0]));
  case VECSXP: return op(Impl<VECSXP, Data>(data, x, Rcpp::Vector<VECSXP>(def)[0]));
  default: break;
  }

  return R_UnboundValue;
}

// first( <column>, default = <*> )
template <typename Data, typename Operation>
SEXP first2_default(const Data& data, SEXP x, SEXP def, const Operation& op) {
  return firstlast_2_default<Data, Operation, internal::First1>(data, x, def, op);
}

// last( <column>, default = <*> )
template <typename Data, typename Operation>
SEXP last2_default(const Data& data, SEXP x, SEXP def, const Operation& op) {
  return firstlast_2_default<Data, Operation, internal::Last1>(data, x, def, op);
}

}
}

#endif
