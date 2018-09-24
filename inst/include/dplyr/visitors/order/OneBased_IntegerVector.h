#ifndef dplyr_OneBased_IntegerVector_H
#define dplyr_OneBased_IntegerVector_H

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

}
#endif
