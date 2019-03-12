#ifndef dplyr__Quosure_h
#define dplyr__Quosure_h

#include <tools/SymbolString.h>
#include <tools/utils.h>
#include "SymbolVector.h"

namespace dplyr {

class Quosure {
public:
  Quosure(SEXP data_) : data(data_) {}

  inline operator SEXP() const {
    return data;
  }

  SEXP expr() const {
    return rlang::quo_get_expr(data);
  }
  SEXP env() const {
    return rlang::quo_get_env(data);
  }

private:
  // quosure typically come from the R side, so don't need
  // further protection, so it's the user responsability to protect
  // them if needed, as in arrange.cpp
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
  const SymbolString& name() const {
    return name_;
  }
  SEXP get() const {
    return quosure;
  }

  bool is_rlang_lambda() const {
    SEXP expr_ = expr();
    return TYPEOF(expr_) == LANGSXP && Rf_inherits(CAR(expr_), "rlang_lambda_function");
  }

private:
  Quosure quosure;
  SymbolString name_;
};

class LambdaQuosure {
public:
  LambdaQuosure(const NamedQuosure& named_quosure, SEXP data_mask) :
    quosure(make_lambda_quosure(named_quosure, data_mask))
  {}

  const NamedQuosure& get() const {
    return quosure;
  }

  ~LambdaQuosure() {
    UNPROTECT(2);
  }

private:

  NamedQuosure make_lambda_quosure(const NamedQuosure& named_quosure, SEXP data_mask) {
    // need to create a new quosure to put the data mask in scope
    // of the lambda function
    SEXP expr = PROTECT(Rf_duplicate(named_quosure.expr()));
    SET_CLOENV(CAR(expr), data_mask) ;
    Rcpp::Shield<SEXP> named_quosure_env(named_quosure.env());
    return NamedQuosure(
             PROTECT(rlang::new_quosure(expr, named_quosure_env)),
             named_quosure.name()
           );
  }

  LambdaQuosure(const LambdaQuosure&) ;

  NamedQuosure quosure;
};



} // namespace dplyr

namespace dplyr {

class QuosureList {
public:
  QuosureList(const Rcpp::List& data_) : data() {
    int n = data_.size();
    if (n == 0) return;

    data.reserve(n);

    Rcpp::CharacterVector names = data_.names();
    for (int i = 0; i < n; i++) {
      SEXP x = data_[i];

      if (!rlang::is_quosure(x)) {
        Rcpp::stop("corrupt tidy quote");
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
    Rcpp::CharacterVector out(data.size());

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
