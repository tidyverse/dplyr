#ifndef dplyr__Quosure_h
#define dplyr__Quosure_h

#include <tools/SymbolString.h>
#include <tools/utils.h>
#include "SymbolVector.h"

namespace dplyr {

class Quosure {
public:
  Quosure(SEXP data_) : data(data_) {}

  SEXP expr() const {
    return internal::rlang_api().quo_get_expr(data);
  }
  SEXP env() const {
    return internal::rlang_api().quo_get_env(data);
  }

private:
  // quosures all come directly from R, so they don't need protection
  SEXP data;
};

class NamedQuosure {
public:
  NamedQuosure(SEXP data_, SymbolString name__) :
    quosure(data_),
    name_(name__)
  {}

  SEXP expr() const {
    return quosure.expr();
  }
  SEXP env() const {
    return quosure.env();
  }
  SymbolString name() const {
    return name_;
  }

private:
  Quosure quosure;
  SymbolString name_;
};

} // namespace dplyr

namespace dplyr {

class QuosureList {
public:
  QuosureList(const List& data_) : data() {
    int n = data_.size();
    if (n == 0) return;

    data.reserve(n);

    CharacterVector names = data_.names();
    for (int i = 0; i < n; i++) {
      SEXP x = data_[i];

      if (!dplyr::internal::rlang_api().is_quosure(x)) {
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
