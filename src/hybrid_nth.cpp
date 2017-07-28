#include "pch.h"
#include <dplyr/main.h>

#include <dplyr/Order.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/Processor.h>
#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/VectorSliceVisitor.h>

using namespace Rcpp;
using namespace dplyr;

namespace dplyr {

bool argmatch(const std::string& target, const std::string& s) {
  if (s.size() > target.size()) return false;
  return target.compare(0, s.size(), s) == 0;
}

template <int RTYPE>
class Nth : public Processor< RTYPE, Nth<RTYPE> > {
public:
  typedef Processor< RTYPE, Nth<RTYPE> >  Base;
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  Nth(Vector<RTYPE> data_, int idx_, STORAGE def_ = Vector<RTYPE>::get_na()) :
    Base(data_),
    data(data_),
    idx(idx_),
    def(def_) {}

  inline STORAGE process_chunk(const SlicingIndex& indices) {
    int n = indices.size();
    if (n == 0 || idx > n || idx < -n) return def;
    int i = idx > 0 ? (idx - 1) : (n + idx);
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

  NthWith(Vector<RTYPE> data_, int idx_, Vector<ORDER_RTYPE> order_, STORAGE def_ = Vector<RTYPE>::get_na()) :
    Base(data_),
    data(data_),
    idx(idx_),
    order(order_),
    def(def_) {}

  inline STORAGE process_chunk(const SlicingIndex& indices) {
    int n = indices.size();
    if (n == 0 || idx > n || idx < -n) return def;

    int i = idx > 0 ? (idx - 1) : (n + idx);

    typedef VectorSliceVisitor<ORDER_RTYPE> Slice;
    typedef OrderVectorVisitorImpl<ORDER_RTYPE, true, Slice> Visitor;
    typedef Compare_Single_OrderVisitor<Visitor> Comparer;

    // Need explicit variables because constructors take const&, and this does not work
    // with unnamed temporaries.
    Slice slice(order, indices);
    Visitor visitor(slice);
    Comparer comparer(visitor);

    IntegerVector sequence = seq(0, n - 1);
    std::nth_element(sequence.begin(), sequence.begin() + i, sequence.end(), comparer);

    return data[ indices[ sequence[i] ] ];
  }

private:
  Vector<RTYPE> data;
  int idx;
  Vector<ORDER_RTYPE> order;
  STORAGE def;
};

Result* nth_(SEXP data, int idx) {
  switch (TYPEOF(data)) {
  case LGLSXP:
    return new Nth<LGLSXP>(data, idx);
  case INTSXP:
    return new Nth<INTSXP>(data, idx);
  case REALSXP:
    return new Nth<REALSXP>(data, idx);
  case CPLXSXP:
    return new Nth<CPLXSXP>(data, idx);
  case STRSXP:
    return new Nth<STRSXP>(data, idx);
  default:
    return 0;
  }
}

template <int RTYPE>
Result* nth_noorder_default(Vector<RTYPE> data, int idx, Vector<RTYPE> def) {
  return new Nth<RTYPE>(data, idx, def[0]);
}

Result* nth_noorder_default_(SEXP data, int idx, SEXP def) {
  switch (TYPEOF(data)) {
  case LGLSXP:
    return nth_noorder_default<LGLSXP>(data, idx, def);
  case INTSXP:
    return nth_noorder_default<INTSXP>(data, idx, def);
  case REALSXP:
    return nth_noorder_default<REALSXP>(data, idx, def);
  case CPLXSXP:
    return nth_noorder_default<CPLXSXP>(data, idx, def);
  case STRSXP:
    return nth_noorder_default<STRSXP>(data, idx, def);
  default:
    return 0;
  }
}

template <int RTYPE>
Result* nth_with(Vector<RTYPE> data, int idx, SEXP order) {
  switch (TYPEOF(order)) {
  case LGLSXP:
    return new NthWith<RTYPE, LGLSXP>(data, idx, order);
  case INTSXP:
    return new NthWith<RTYPE, INTSXP>(data, idx, order);
  case REALSXP:
    return new NthWith<RTYPE, REALSXP>(data, idx, order);
  case CPLXSXP:
    return new NthWith<RTYPE, CPLXSXP>(data, idx, order);
  case STRSXP:
    return new NthWith<RTYPE, STRSXP>(data, idx, order);
  default:
    break;
  }
  bad_arg(SymbolString("order"), "is of unsupported type %s", Rf_type2char(TYPEOF(order)));
}

Result* nth_with_(SEXP data, int idx, SEXP order_by) {
  switch (TYPEOF(data)) {
  case LGLSXP:
    return nth_with<LGLSXP>(data, idx, order_by);
  case INTSXP:
    return nth_with<INTSXP>(data, idx, order_by);
  case REALSXP:
    return nth_with<REALSXP>(data, idx, order_by);
  case CPLXSXP:
    return nth_with<CPLXSXP>(data, idx, order_by);
  case STRSXP:
    return nth_with<STRSXP>(data, idx, order_by);
  default:
    return 0;
  }
}

template <int RTYPE>
Result* nth_with_default(Vector<RTYPE> data, int idx, SEXP order, Vector<RTYPE> def) {
  switch (TYPEOF(order)) {
  case LGLSXP:
    return new NthWith<RTYPE, LGLSXP>(data, idx, order, def[0]);
  case INTSXP:
    return new NthWith<RTYPE, INTSXP>(data, idx, order, def[0]);
  case REALSXP:
    return new NthWith<RTYPE, REALSXP>(data, idx, order, def[0]);
  case CPLXSXP:
    return new NthWith<RTYPE, CPLXSXP>(data, idx, order, def[0]);
  case STRSXP:
    return new NthWith<RTYPE, STRSXP>(data, idx, order, def[0]);
  default:
    break;
  }
  bad_arg(SymbolString("order"), "is of unsupported type %s", Rf_type2char(TYPEOF(order)));
}

Result* nth_with_default_(SEXP data, int idx, SEXP order_by, SEXP def) {
  switch (TYPEOF(data)) {
  case LGLSXP:
    return nth_with_default<LGLSXP>(data, idx, order_by, def);
  case INTSXP:
    return nth_with_default<INTSXP>(data, idx, order_by, def);
  case REALSXP:
    return nth_with_default<REALSXP>(data, idx, order_by, def);
  case CPLXSXP:
    return nth_with_default<CPLXSXP>(data, idx, order_by, def);
  case STRSXP:
    return nth_with_default<STRSXP>(data, idx, order_by, def);
  default:
    return 0;
  }
}

Result* nth_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  // has to have at least two arguments
  if (nargs < 2) return 0;

  SEXP tag = TAG(CDR(call));
  if (tag != R_NilValue && tag != Rf_install("x")) {
    return 0;
  }
  SEXP data = maybe_rhs(CADR(call));
  if (TYPEOF(data) != SYMSXP)
    return 0;

  SymbolString name = SymbolString(Symbol(data));
  if (subsets.has_non_summary_variable(name) == 0) {
    return 0;
  }
  data = subsets.get_variable(name);

  tag = TAG(CDDR(call));
  if (tag != R_NilValue && tag != Rf_install("n")) {
    return 0;
  }
  SEXP nidx = CADDR(call);
  if ((TYPEOF(nidx) != REALSXP && TYPEOF(nidx) != INTSXP) || LENGTH(nidx) != 1) {
    // we only know how to handle the case where nidx is a length one
    // integer or numeric. In any other case, e.g. an expression for R to evaluate
    // we just fallback to R evaluation (#734)
    return 0;
  }
  int idx = as<int>(nidx);

  // easy case : just a single variable: first(x,n)
  if (nargs == 2) {
    return nth_(data, idx);
  }

  // now get `order_by` and `default`
  SEXP order_by = R_NilValue;
  SEXP def    = R_NilValue;
  bool has_order_by = false;
  bool has_default = false;

  SEXP p = CDR(CDDR(call));
  while (p != R_NilValue) {
    SEXP tag = TAG(p);
    if (!has_order_by && (Rf_isNull(tag) || argmatch("order_by", CHAR(PRINTNAME(tag))))) {
      order_by = CAR(p);
      has_order_by = true;
    }
    else if (!has_default && (Rf_isNull(tag) || argmatch("default", CHAR(PRINTNAME(tag))))) {
      def = CAR(p);
      has_default = true;
    }
    else {
      return 0;
    }

    p = CDR(p);
  }

  // handle cases
  if (Rf_isNull(def)) {
    // then we know order_by is not NULL, we only handle the case where
    // order_by is a symbol and that symbol is in the data
    if (TYPEOF(order_by) != SYMSXP)
      return 0;

    SymbolString order_by_name = SymbolString(Symbol(order_by));
    if (subsets.has_non_summary_variable(order_by_name) == 0)
      return 0;

    order_by = subsets.get_variable(order_by_name);

    return nth_with_(data, idx, order_by);
  }

  if (Rf_isNull(order_by)) {
    return nth_noorder_default_(data, idx, def);
  }

  if (TYPEOF(order_by) != SYMSXP)
    return 0;

  SymbolString order_by_name = SymbolString(Symbol(order_by));
  if (subsets.has_non_summary_variable(order_by_name) == 0)
    return 0;

  order_by = subsets.get_variable(order_by_name);

  return nth_with_default_(data, idx, order_by, def);
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
