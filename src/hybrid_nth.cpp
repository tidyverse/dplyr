#include <dplyr/main.h>

#include <dplyr/Order.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/Processor.h>
#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/VectorSliceVisitor.h>

#include <tools/constfold.h>
#include <tools/match.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

  template <int RTYPE>
  inline typename Rcpp::traits::storage_type<RTYPE>::type init_def(SEXP def) {
    if (Rf_isNull(def))
      return Vector<RTYPE>::get_na();

    Vector<RTYPE> def_ = def;

    if (def_.length() != 1)
      stop("length 1 expected");

    return def_[0];
  }

  template <int RTYPE>
  class Nth : public Processor< RTYPE, Nth<RTYPE> > {
  public:
    typedef Processor< RTYPE, Nth<RTYPE> >  Base;
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

    Nth(const Vector<RTYPE>& data_, int idx_, SEXP def_) :
      Base(data_),
      data(data_),
      idx(idx_),
      def(init_def<RTYPE>(def_)) {}

    inline STORAGE process_chunk(const SlicingIndex& indices) {
      int n = indices.size();
      if (n == 0 || idx > n || idx < -n) return def;
      int i = idx > 0 ? (idx -1) : (n+idx);
      return data[indices[i]];
    }

  private:
    Vector<RTYPE> data;
    int idx;
    STORAGE def;
  };

  template <int RTYPE, int ORDER_RTYPE>
  class NthWith : public Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > {
  public:
    typedef Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > Base;
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

    NthWith(const Vector<RTYPE>& data_, int idx_, Vector<ORDER_RTYPE> order_, SEXP def_) :
      Base(data_),
      data(data_),
      idx(idx_),
      order(order_),
      def(init_def<RTYPE>(def_)) {}

    inline STORAGE process_chunk(const SlicingIndex& indices) {
      int n = indices.size();
      if (n == 0 || idx > n || idx < -n) return def;

      int i = idx > 0 ? (idx -1) : (n+idx);

      typedef VectorSliceVisitor<ORDER_RTYPE> Slice;
      typedef OrderVectorVisitorImpl<ORDER_RTYPE,true,Slice> Visitor;
      typedef Compare_Single_OrderVisitor<Visitor> Comparer;

      Comparer comparer(Visitor(Slice(order, indices)));
      IntegerVector sequence = seq(0,n-1);
      std::nth_element(sequence.begin(), sequence.begin() + i, sequence.end(), comparer);

      return data[ indices[ sequence[i] ] ];
    }

  private:
    Vector<RTYPE> data;
    int idx;
    Vector<ORDER_RTYPE> order;
    STORAGE def;
  };

  template <int RTYPE>
  Result* nth_natural_(Vector<RTYPE> data, int idx, SEXP def) {
    try {
      return new Nth<RTYPE>(data, idx, def);
    }
    catch (...) {
      return 0;
    }
  }

  Result* nth_natural(SEXP data, int idx, SEXP def) {
    switch (TYPEOF(data)) {
    case LGLSXP:
      return nth_natural_<LGLSXP>(data, idx, def);
    case INTSXP:
      return nth_natural_<INTSXP>(data, idx, def);
    case REALSXP:
      return nth_natural_<REALSXP>(data, idx, def);
    case CPLXSXP:
      return nth_natural_<CPLXSXP>(data, idx, def);
    case STRSXP:
      return nth_natural_<STRSXP>(data, idx, def);
    default:
      break;
    }
    return 0;
  }

  template <int RTYPE, int ORDER_RTYPE>
  Result* nth_ordered__(const Vector<RTYPE>& data, int idx, Vector<ORDER_RTYPE> order, SEXP def) {
    try {
      return new NthWith<RTYPE, ORDER_RTYPE>(data, idx, order, def);
    }
    catch (...) {
      return 0;
    }
  }

  template <int RTYPE>
  Result* nth_ordered_(Vector<RTYPE> data, int idx, SEXP order, SEXP def) {
    switch (TYPEOF(order)) {
    case LGLSXP:
      return nth_ordered__<RTYPE, LGLSXP>(data, idx, order, def);
    case INTSXP:
      return nth_ordered__<RTYPE, INTSXP>(data, idx, order, def);
    case REALSXP:
      return nth_ordered__<RTYPE, REALSXP>(data, idx, order, def);
    case STRSXP:
      return nth_ordered__<RTYPE, STRSXP>(data, idx, order, def);
    default:
      break;
    }
    return 0;
  }

  Result* nth_ordered(SEXP data, int idx, SEXP order, SEXP def) {
    switch (TYPEOF(data)) {
    case LGLSXP:
      return nth_ordered_<LGLSXP>(data, idx, order, def);
    case INTSXP:
      return nth_ordered_<INTSXP>(data, idx, order, def);
    case REALSXP:
      return nth_ordered_<REALSXP>(data, idx, order, def);
    case CPLXSXP:
      return nth_ordered_<CPLXSXP>(data, idx, order, def);
    case STRSXP:
      return nth_ordered_<STRSXP>(data, idx, order, def);
    default:
      break;
    }
    return 0;
  }

  SEXP get_nth() {
    static Function nth("nth", Environment::namespace_env("dplyr"));
    return nth;
  }

  Result* nth_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
    // has to have at least two arguments
    if (nargs < 2) {
      LOG_VERBOSE;
      return 0;
    }

    call = r_match_call(get_nth(), call);

    SEXP p = CDR(call);
    SEXP tag = TAG(p);
    if (tag != Rf_install("x")) {
      LOG_VERBOSE;
      return 0;
    }
    SEXP data = CAR(p);
    if (TYPEOF(data) == SYMSXP) {
      if (! subsets.count(data)) {
        LOG_VERBOSE;
        return 0;
      }
      data = subsets.get_variable(data);
    }
    else {
      LOG_VERBOSE;
      return 0;
    }

    p = CDR(p);
    tag = TAG(p);

    if (tag != Rf_install("n")) {
      LOG_VERBOSE;
      return 0;
    }

    SEXP nidxe = CAR(p);
    SEXP nidx = r_constfold(nidxe);
    // we only know how to handle the case where nidx is a length one
    // integer or numeric (after constant folding). In any other case
    // we just fallback to R evaluation (#734)
    int idx;
    if (!is<int>(nidx) && !is<double>(nidx)) {
      LOG_VERBOSE;
      return 0;
    }
    idx = as<int>(nidx);

    p = CDR(p);
    tag = TAG(p);

    SEXP order_by = R_NilValue;
    SEXP def    = R_NilValue;

    // now get `order_by` and default
    if (tag == Rf_install("order_by")) {
      order_by = CAR(p);

      p = CDR(p);
      tag = TAG(p);
    }

    if (tag == Rf_install("default")) {
      SEXP defe = CAR(p);
      def = r_constfold(defe);

      p = CDR(p);
      tag = TAG(p);
    }

    if (!Rf_isNull(p)) {
      LOG_VERBOSE;
      return 0;
    }

    if (Rf_isNull(order_by)) {
      return nth_natural(data, idx, def);
    }
    else {
      return nth_ordered(data, idx, order_by, def);
    }
  }

  Result* firstlast_prototype(SEXP call, const ILazySubsets& subsets, int nargs, int pos) {
    SEXP tail = CDDR(call);

    SETCAR(call, Rf_install("nth"));

    Pairlist p(pos);
    if (Rf_isNull(tail)) {
      SETCDR(CDR(call), p);
    } else {
      SETCDR(p, tail);
      SETCDR(CDR(call), p);
    }
    Result* res = nth_prototype(call, subsets, nargs + 1);
    return res;
  }

  Result* first_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
    return firstlast_prototype(call, subsets, nargs, 1);
  }

  Result* last_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
    return firstlast_prototype(call, subsets, nargs, -1);
  }

}

void install_nth_handlers(HybridHandlerMap& handlers) {
  handlers[ Rf_install("first") ] = first_prototype;
  handlers[ Rf_install("last") ] = last_prototype;
  handlers[ Rf_install("nth") ] = nth_prototype;
}
