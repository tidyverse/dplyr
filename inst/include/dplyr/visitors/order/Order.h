#ifndef dplyr_Order_H
#define dplyr_Order_H

#include <tools/pointer_vector.h>
#include <dplyr/visitors/order/OrderVisitorImpl.h>

namespace dplyr {

class OneBased_IntegerVector {
public:
  OneBased_IntegerVector(const Rcpp::IntegerVector& data_) :
    data(data_)
  {}

  // used when we do C++ subscripting
  inline int operator[](int i) const {
    return data[i] - 1;
  }

  // used when we do R subscripting
  inline operator SEXP() const {
    return data;
  }

  inline R_xlen_t size() const {
    return data.size();
  }

private:
  Rcpp::IntegerVector data;
};

class OrderVisitors {
private:

  class Compare {
  public:
    Compare(const OrderVisitors& obj_) :
      obj(obj_)
    {}

    inline bool operator()(int i, int j) const {
      if (i == j) return false;
      for (int k = 0; k < obj.n; k++) {
        if (! obj.visitors[k]->equal(i - 1, j - 1)) {
          return obj.visitors[k]->before(i - 1, j - 1);
        }
      }
      return i < j;
    }

  private:
    const OrderVisitors& obj;
  };

public:

  OrderVisitors(List args, LogicalVector ascending, int n_) :
    visitors(n_),
    n(n_),
    nrows(0)
  {
    nrows = Rf_length(args[0]);
    for (int i = 0; i < n; i++) {
      visitors[i]  = order_visitor(args[i], ascending[i], i);
    }
  }

  inline OneBased_IntegerVector apply() const {
    if (nrows == 0) return IntegerVector(0);
    IntegerVector x = seq(1, nrows);
    std::sort(x.begin(), x.end(), Compare(*this));
    return OneBased_IntegerVector(x);
  }

  pointer_vector<OrderVisitor> visitors;
  int n;
  int nrows;

private:
  OrderVisitors(const OrderVisitors&);
};

} // namespace dplyr


#endif
