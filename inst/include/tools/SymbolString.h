#ifndef dplyr_tools_SymbolString_h
#define dplyr_tools_SymbolString_h

namespace Rcpp {

  class SymbolString  {
  public:
    SymbolString() {}

    SymbolString(const String& other) : s(other) {}

    SymbolString(const String::StringProxy& other) : s(other) {}

    SymbolString(const String::const_StringProxy& other) : s(other) {}

    // Symbols are always encoded in the native encoding (#1950)
    explicit SymbolString(const Symbol& symbol) : s(CHAR(PRINTNAME(symbol)), CE_NATIVE) {}

  public:
    const String& get_string() const {
      return s;
    }

    std::string get_cstring() const {
      return s.get_cstring();
    }

    SEXP get_sexp() const {
      return s.get_sexp();
    }

  private:
    String s;
  };

}

#endif
