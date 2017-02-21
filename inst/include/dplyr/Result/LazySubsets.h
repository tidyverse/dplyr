#ifndef dplyr_LazySubsets_H
#define dplyr_LazySubsets_H

#include <tools/SymbolMap.h>
#include <tools/SlicingIndex.h>
#include <dplyr/Result/ILazySubsets.h>

namespace dplyr {

  class LazySubsets : public ILazySubsets {
  public:
    LazySubsets(const DataFrame& df) : nr(df.nrows()) {
      int nvars = df.size();
      if (nvars) {
        CharacterVector names = df.names();
        for (int i=0; i<nvars; i++) {
          SEXP column = df[i];
          if (Rf_inherits(column, "matrix")) {
            stop("matrix as column is not supported");
          }
          symbol_map.insert(names[i]);
          data.push_back(df[i]);
        }
      }
    }
    virtual ~LazySubsets() {}

  public:
    virtual CharacterVector get_variable_names() const {
      return symbol_map.get_names();
    }

    virtual SEXP get_variable(SEXP symbol) const {
      return data[ symbol_map.get(symbol) ];
    }

    virtual SEXP get(SEXP symbol, const SlicingIndex& indices) const {
      const int pos = symbol_map.get(symbol);
      SEXP col = data[pos];
      if (!indices.is_identity(col) && Rf_length(col) != 1)
        stop("Attempt to query lazy column with non-natural slicing index");

      return col;
    }

    virtual bool is_summary(SEXP symbol) const {
      return summary_map.has(symbol);
    }

    virtual int count(SEXP symbol) const {
      int res = symbol_map.has(symbol);
      return res;
    }

    virtual void input(SEXP symbol, SEXP x) {
      SymbolMapIndex index = symbol_map.insert(symbol);
      if (index.origin == NEW) {
        data.push_back(x);
      } else {
        data[index.pos] = x;
      }
      summary_map.insert(symbol);
    }

    virtual int size() const {
      return data.size();
    }

    virtual int nrows() const {
      return nr;
    }

  public:
    void clear() {}

    inline SEXP& operator[](SEXP symbol) {
      return data[symbol_map.get(symbol)];
    }

  private:
    SymbolMap symbol_map, summary_map;
    std::vector<SEXP> data;
    int nr;
  };

}

#endif
