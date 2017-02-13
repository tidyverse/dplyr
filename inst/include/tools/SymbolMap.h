#ifndef dplyr_tools_SymbolMap_h
#define dplyr_tools_SymbolMap_h

#include <tools/hash.h>
#include <tools/match.h>

namespace dplyr {

  enum Origin { HASH, RMATCH, NEW };

  struct SymbolMapIndex {
    int pos;
    Origin origin;

    SymbolMapIndex(int pos_, Origin origin_) :
      pos(pos_), origin(origin_)
    {}
  };

  class SymbolMap {
  private:
    dplyr_hash_map<SEXP, int> lookup;
    CharacterVector names;

  public:
    SymbolMap(): lookup(), names() {}

    SymbolMapIndex insert(const SymbolString& name) {
      SymbolMapIndex index = get_index(name);
      int idx = index.pos;
      switch (index.origin) {
      case HASH:
        break;
      case RMATCH:
        lookup.insert(std::make_pair(name.get_sexp(), idx));
        break;
      case NEW:
        names.push_back(name);
        lookup.insert(std::make_pair(name.get_sexp(), idx));
        break;
      };
      return index;
    }

    CharacterVector get_names() const {
      return names;
    }

    int size() const {
      return names.size();
    }

    bool has(const SymbolString& name) const {
      SymbolMapIndex index = get_index(name);
      return index.origin != NEW;
    }

    SymbolMapIndex get_index(const SymbolString& name) const {
      // first, lookup the map
      dplyr_hash_map<SEXP, int>::const_iterator it = lookup.find(name.get_sexp());
      if (it != lookup.end()) {
        return SymbolMapIndex(it->second, HASH);
      }

      CharacterVector v = CharacterVector::create(name);
      int idx = as<int>(r_match(v, names));
      if (idx != NA_INTEGER) {
        // we have a match
        return SymbolMapIndex(idx-1, RMATCH);
      }

      // no match
      return SymbolMapIndex(names.size(), NEW);
    }

    int get(const SymbolString& name) const {
      SymbolMapIndex index = get_index(name);
      if (index.origin == NEW) {
        stop("variable '%s' not found", name.get_cstring());
      }
      return index.pos;
    }

    SymbolMapIndex rm(const SymbolString& name) {
      SymbolMapIndex index = get_index(name);
      if (index.origin != NEW) {
        int idx = index.pos;
        names.erase(names.begin() + idx);

        for (dplyr_hash_map<SEXP, int>::iterator it=lookup.begin(); it != lookup.end();) {
          int k = it->second;

          if (k < idx) {
            // nothing to do in that case
            ++it;
            continue;
          } else if (k == idx) {
            // need to remove the data from the hash table
            it = lookup.erase(it);
            continue;
          } else {
            // decrement the index
            it->second--;
            ++it;
          }
        }

      }

      return index;
    }

  };

}

#endif
