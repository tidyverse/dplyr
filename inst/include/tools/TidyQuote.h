#ifndef dplyr__TidyQuote_h
#define dplyr__TidyQuote_h

#include <tools/SymbolString.h>


namespace dplyr {

class TidyQuote {
 public:
  TidyQuote(const Formula& data_, SymbolString name__) :
      data(data_),
      name_(name__)
  {}

  TidyQuote(const TidyQuote& other) :
      data(other.data),
      name_(other.name_)
  {}

  SEXP expr() const {
    return Rf_duplicate(CADR(data));
  }
  SEXP env() const {
    return Rf_getAttrib(data, Rf_install(".Environment"));
  }
  SymbolString name() const {
    return name_;
  }

 private:
  Formula data;
  SymbolString name_;
};

} // namespace dplyr


namespace Rcpp {

using namespace dplyr;

template <>
inline bool is<TidyQuote>(SEXP x) {
  bool is_tilde =
    TYPEOF(x) == LANGSXP &&
    Rf_length(x) == 2 &&
    CAR(x) == Rf_install("~");

  SEXP env = Rf_getAttrib(x, Rf_install(".Environment"));
  bool has_env = TYPEOF(env) == ENVSXP;

  return is_tilde && has_env;
}

} // namespace Rcpp


namespace dplyr {

class TidyQuotes {
 public:
  TidyQuotes(const List& data_) : data() {
    int n = data_.size();
    if (n == 0) return;

    CharacterVector names = data_.names();
    for (int i=0; i<n; i++) {
      SEXP x = data_[i];

      if (!is<TidyQuote>(x)) {
        stop("corrupt tidy quote");
      }

      data.push_back(TidyQuote(x, SymbolString(names[i])));
    }
  }

  const TidyQuote& operator[](int i) const {
    return data[i];
  }

  int size() const {
    return data.size();
  }

  bool single_env() const {
    if (data.size() <= 1) return true;
    SEXP env = data[0].env();
    for (size_t i=1; i<data.size(); i++) {
      if (data[i].env() != env) return false;
    }
    return true;
  }

 private:
  std::vector<TidyQuote> data;
};

} // namespace dplyr

#endif
