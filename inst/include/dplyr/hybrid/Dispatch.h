#ifndef dplyr_hybrid_dispatch_h
#define dplyr_hybrid_dispatch_h

#include <dplyr/hybrid/HybridVectorScalarResult.h>
#include <dplyr/hybrid/Column.h>

namespace dplyr {
namespace hybrid {

struct Summary {
  template <typename T>
  inline SEXP operator()(const T& obj) const {
    return obj.summarise();
  }
};

struct Window {
  template <typename T>
  inline SEXP operator()(const T& obj) const {
    return obj.window();
  }
};

struct Match {
  template <typename T>
  inline SEXP operator()(const T& obj) const {
    return Rf_mkString(DEMANGLE(T));
  }
};

template <int RTYPE, bool NA_RM, typename Data, template <int, bool, typename> class Impl >
class SimpleDispatchImpl : public HybridVectorScalarResult < RTYPE == LGLSXP ? INTSXP : RTYPE, Data, SimpleDispatchImpl<RTYPE, NA_RM, Data, Impl> > {
public :
  static const int rtype = RTYPE == LGLSXP ? INTSXP : RTYPE;
  typedef typename Rcpp::Vector<RTYPE>::stored_type STORAGE;

  typedef HybridVectorScalarResult<rtype, Data, SimpleDispatchImpl > Parent ;
  typedef typename Data::slicing_index Index;

  SimpleDispatchImpl(const Data& data, Column vec) :
    Parent(data),
    data_ptr(Rcpp::internal::r_vector_start<RTYPE>(vec.data)),
    is_summary(vec.is_summary)
  {}

  STORAGE process(const Index& indices) const {
    return Impl<RTYPE, NA_RM, Index>::process(data_ptr, indices, is_summary);
  }

private:
  STORAGE* data_ptr;
  bool is_summary;
} ;

template <
  typename Data,
  template <int, bool, typename> class Impl
  >
class SimpleDispatch {
public:
  typedef typename Data::slicing_index Index;

  SimpleDispatch(const Data& data_, Column variable_, bool narm_):
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
      return op(SimpleDispatchImpl<INTSXP, NARM, Data, Impl>(data, variable));
    case REALSXP:
      return op(SimpleDispatchImpl<REALSXP, NARM, Data, Impl>(data, variable));
    case LGLSXP:
      return op(SimpleDispatchImpl<LGLSXP, NARM, Data, Impl>(data, variable));
    }

    // give up, effectively let R evaluate the call
    return R_UnboundValue;
  }

};

}
}


#endif
