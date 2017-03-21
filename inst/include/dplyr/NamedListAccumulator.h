#ifndef dplyr_NamedListAccumulator_H
#define dplyr_NamedListAccumulator_H

#include <tools/SymbolMap.h>

#include <dplyr/checks.h>

namespace dplyr {

  template <typename Data>
  class NamedListAccumulator {
  public:
    SymbolMap symbol_map;
    std::vector<SEXP> data;

    NamedListAccumulator() {}

    inline void set(const SymbolString& name, SEXP x) {
      if (! Rcpp::traits::same_type<Data, RowwiseDataFrame>::value)
        check_supported_type(x, name);

      if (TYPEOF(x) != VECSXP)
        Rf_setAttrib(x, R_NamesSymbol, R_NilValue);

      SymbolMapIndex index = symbol_map.insert(name);
      if (index.origin == NEW) {
        data.push_back(x);
      } else {
        data[ index.pos ] = x;
      }

    }

    inline void rm(const SymbolString& name) {
      SymbolMapIndex index = symbol_map.rm(name);
      if (index.origin != NEW) {
        data.erase(data.begin() + index.pos);
      }
    }

    inline operator List() const {
      List out = wrap(data);
      out.names() = symbol_map.get_names();
      return out;
    }

    inline size_t size() const {
      return data.size();
    }

    inline const SymbolVector names() const {
      return symbol_map.get_names();
    }

  };

}
#endif
