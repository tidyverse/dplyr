#ifndef dplyr_tools_SymbolVector_h
#define dplyr_tools_SymbolVector_h

#include <tools/SymbolString.h>

namespace Rcpp {

  class SymbolVector : public CharacterVector {
  public:
    SymbolVector() {}

    SymbolVector(const SymbolVector& s) : CharacterVector(s) {}

    template <class T>
    explicit SymbolVector(T s) : CharacterVector(s) {}
  };

}

#endif
