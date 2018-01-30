#ifndef dplyr__Quosure_h
#define dplyr__Quosure_h

#include <tools/SymbolString.h>
#include <tools/utils.h>
#include "SymbolVector.h"


namespace dplyr {

inline SEXP quosure(SEXP expr, SEXP env) {
  return internal::rlang_api().new_quosure(expr, env);
}


class NamedQuosure {
public:
  NamedQuosure(SEXP data_, SymbolString name__ = "") :
    data(data_),
    name_(name__)
  {}
  NamedQuosure(const Formula& data_, SymbolString name__ = "") :
    data(data_),
    name_(name__)
  {}
  NamedQuosure(const NamedQuosure& other) :
    data(other.data),
    name_(other.name_)
  {}

  SEXP expr() const {
    return Rf_duplicate(internal::rlang_api().quo_get_expr(data));
  }
  SEXP env() const {
    return internal::rlang_api().quo_get_env(data);
  }
  SymbolString name() const {
    return name_;
  }

private:
  RObject data;
  SymbolString name_;
};

} // namespace dplyr


namespace Rcpp {

using namespace dplyr;

template <>
inline bool is<NamedQuosure>(SEXP x) {
  return dplyr::internal::rlang_api().is_quosure(x);
}

} // namespace Rcpp


namespace dplyr {

class QuosureList {
public:
  QuosureList(const List& data_) : data() {
    int n = data_.size();
    if (n == 0) return;

    CharacterVector names = data_.names();
    for (int i = 0; i < n; i++) {
      SEXP x = data_[i];

      if (!is<NamedQuosure>(x)) {
        stop("corrupt tidy quote");
      }

      data.push_back(NamedQuosure(x, SymbolString(names[i])));
    }
  }

  const NamedQuosure& operator[](int i) const {
    return data[i];
  }

  int size() const {
    return data.size();
  }

  bool single_env() const {
    if (data.size() <= 1) return true;
    SEXP env = data[0].env();
    for (size_t i = 1; i < data.size(); i++) {
      if (data[i].env() != env) return false;
    }
    return true;
  }

  SymbolVector names() const {
    CharacterVector out(data.size());

    for (size_t i = 0; i < data.size(); ++i) {
      out[i] = data[i].name().get_string();
    }

    return SymbolVector(out);
  }

private:
  std::vector<NamedQuosure> data;
};

} // namespace dplyr

#endif
