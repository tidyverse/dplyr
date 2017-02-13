#ifndef dplyr_tools_SymbolString_h
#define dplyr_tools_SymbolString_h

namespace Rcpp {

  class SymbolString : public String {
  public:
    SymbolString() {}

    SymbolString(const String& other) : String(other) {}

    SymbolString(const StringProxy& other) : String(other) {}

    SymbolString(const const_StringProxy& other) : String(other) {}

    // Symbols are always encoded in the native encoding (#1950)
    explicit SymbolString(const Symbol& s) : String(CHAR(PRINTNAME(s)), CE_NATIVE) {}
  };

}

#endif
