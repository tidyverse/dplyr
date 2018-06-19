#ifndef dplyr_hybrid_dispatch_h
#define dplyr_hybrid_dispatch_h

namespace dplyr{
namespace hybrid{

struct Summary {
  template <typename T>
  inline SEXP operator()(const T& obj) const{
    return obj.summarise();
  }
};

struct Window {
  template <typename T>
  inline SEXP operator()(const T& obj) const{
    return obj.window();
  }
};

template <
  typename Data,
  template<int /* RTYPE */, bool /* NA_RM */, typename /* Data*/ > class Impl
>
class SimpleDispatch {
public:
  SimpleDispatch( const Data& data_, SEXP variable_, bool narm_):
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
  SEXP variable;
  bool narm;

  template <typename Operation>
  SEXP operate(const Operation& op) const {
    if (narm) {
      return operate<Operation, true>(op);
    } else {
      return operate<Operation, false>(op);
    }
  }

  template <typename Operation, bool NARM>
  SEXP operate(const Operation& op) const {
    switch(TYPEOF(variable)){
    case INTSXP: return op(Impl<INTSXP, NARM, Data>(data, variable));
    case REALSXP: return op(Impl<REALSXP, NARM, Data>(data, variable));
    case LGLSXP: return op(Impl<LGLSXP, NARM, Data>(data, variable));
    }
    return R_UnboundValue;
  }

};

}
}


#endif
