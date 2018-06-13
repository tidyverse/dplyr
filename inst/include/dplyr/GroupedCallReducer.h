#ifndef dplyr_GroupedCallReducer_H
#define dplyr_GroupedCallReducer_H
#include <dplyr/DataMask.h>

#include <boost/scoped_ptr.hpp>

#include <tools/all_na.h>

#include <dplyr/Result/Result.h>

#include <dplyr/bad.h>
#include <dplyr/DataMask.h>

#include <tools/hash.h>
#include <tools/scalar_type.h>
#include <tools/utils.h>
#include <tools/r_coerce.h>
#include <dplyr/vector_class.h>
#include <dplyr/checks.h>

namespace dplyr {

template <int RTYPE>
bool valid_conversion(int rtype) {
  return rtype == RTYPE;
}

template <>
inline bool valid_conversion<REALSXP>(int rtype) {
  switch (rtype) {
  case REALSXP:
  case INTSXP:
  case LGLSXP:
    return true;
  default:
    break;
  }
  return false;
}

template <>
inline bool valid_conversion<INTSXP>(int rtype) {
  switch (rtype) {
  case INTSXP:
  case LGLSXP:
    return true;
  default:
    break;
  }
  return false;
}

template <int RTYPE>
inline bool valid_promotion(int) {
  return false;
}

template <>
inline bool valid_promotion<INTSXP>(int rtype) {
  return rtype == REALSXP;
}

template <>
inline bool valid_promotion<LGLSXP>(int rtype) {
  return rtype == REALSXP || rtype == INTSXP;
}

inline bool valid_promotion_dispatch(int x, int y) {
  switch (x) {
  case LGLSXP:
    return valid_promotion<LGLSXP>(y);
  case RAWSXP:
    return valid_promotion<RAWSXP>(y);
  case INTSXP:
    return valid_promotion<INTSXP>(y);
  case REALSXP:
    return valid_promotion<REALSXP>(y);
  case CPLXSXP:
    return valid_promotion<CPLXSXP>(y);
  case VECSXP:
    return valid_promotion<VECSXP>(y);
  default:
    break;
  }
  return false;
}

class FactorFlattener {
public:
  typedef dplyr_hash_map<SEXP, int> LevelsMap;

  FactorFlattener(const List& data_, const SymbolString& name_) :
    data(data_),
    name(name_),
    res(no_init(data.size())),
    levels_map()
  {}

  inline SEXP get() {
    // first result
    SEXP first = data[0];
    copy_most_attributes(res, first);
    res[0] = init_levels(first);

    // subsequent results
    int n = data.size();
    for (int i = 1 ; i < n; i++) {
      res[i] = update_levels(data[i]);
    }

    res.attr("levels") = retrieve_levels();
    return res;
  }

private:
  const List& data;
  const SymbolString& name;

  IntegerVector res;
  LevelsMap levels_map;

  int init_levels(SEXP x) {
    CharacterVector levels = get_levels(x);
    int n = levels.size();
    for (int i = 0; i < n; i++) {
      levels_map[levels[i]] = i + 1;
    }
    return as<int>(x);
  }

  int update_levels(SEXP x) {
    CharacterVector lev = get_levels(x);
    int nlevels = levels_map.size();
    int n = lev.size();
    for (int i = 0; i < n; i++) {
      SEXP s = lev[i];
      if (! levels_map.count(s)) {
        levels_map.insert(std::make_pair(s, ++nlevels));
      }
    }

    int val = as<int>(x);
    if (val != NA_INTEGER) val = levels_map[lev[val - 1]];
    return val;
  }

  CharacterVector retrieve_levels() {
    int n = levels_map.size();
    CharacterVector levels(n);
    LevelsMap::iterator it = levels_map.begin();
    for (int i = 0; i < n; i++, ++it) {
      levels[it->second - 1] = it->first;
    }
    return levels;
  }

};

template <int RTYPE>
class Flattener {
public:
  typedef typename Vector<RTYPE>::stored_type stored_type ;

  Flattener(const List& data_) : data(data_) {}

  inline SEXP get() {
    int n = data.size();
    Rcpp::Vector<RTYPE> res = no_init(n);
    copy_most_attributes(res, data[0]);
    for (int i = 0; i < n; i++) {
      res[i] = coerce(data[i]);
    }
    return res;
  }

private:
  const List& data;

  inline stored_type coerce(SEXP x) {
    switch (TYPEOF(x)) {
    case LGLSXP:
      return Rcpp::internal::r_coerce<LGLSXP, RTYPE>(LOGICAL(x)[0]);
    case RAWSXP:
      return Rcpp::internal::r_coerce<RAWSXP, RTYPE>(RAW(x)[0]);
    case INTSXP:
      return Rcpp::internal::r_coerce<INTSXP, RTYPE>(INTEGER(x)[0]);
    case REALSXP:
      return Rcpp::internal::r_coerce<REALSXP, RTYPE>(REAL(x)[0]);
    case CPLXSXP:
      return Rcpp::internal::r_coerce<CPLXSXP, RTYPE>(COMPLEX(x)[0]);
    case STRSXP:
      return Rcpp::internal::r_coerce<STRSXP, RTYPE>(STRING_ELT(x, 0));
    default:
      break;
    }
    return stored_type();
  }
};

class ListFlattener {
public:
  ListFlattener(const List& data_) : data(data_) {}

  inline SEXP get() {
    int n = data.size();
    List res(n);
    for (int i = 0; i < n; i++) res[i] = VECTOR_ELT(data[i], 0);
    return res;
  }

private:
  const List& data;
};

template <typename Data>
class GroupedCallReducer  {
public:
  typedef typename Data::slicing_index Index;
  typedef typename Data::group_iterator Iterator;

  GroupedCallReducer(SEXP expr_, SymbolString name_, DataMask<Data>& data_mask_) :
    expr(expr_),
    name(name_),
    data_mask(data_mask_)
  {}

  SEXP process(const Data& gdf) {
    int ng = gdf.ngroups();

    // special cases
    if (ng == 0) {
      return LogicalVector(0);
    }

    if (ng == 1) {
      Shield<SEXP> first_result(process_chunk(*gdf.group_begin()));
      check_supported_type(first_result, name);
      check_length(Rf_length(first_result), 1, "a summary value", name);
      return first_result;
    }

    // first collect all the results in a list
    List res(ng);
    Iterator git = gdf.group_begin();
    for (int i = 0; i < ng; i++, ++git) {
      SEXP x = res[i] = process_chunk(*git);

      check_supported_type(x, name);
      check_length(Rf_length(x), 1, "a summary value", name);
    }

    return flatten(res);
  }

  inline SEXP process_chunk(const Index& indices) {
    return data_mask.eval(expr, indices);
  }

  const SymbolString& get_name() const {
    return name;
  }

private:
  SEXP expr;
  const SymbolString name;
  DataMask<Data> data_mask;

  inline SEXP flatten(const List& data) {
    SEXP first_result = data[0];

    check_supported_type(first_result, name);
    check_length(Rf_length(first_result), 1, "a summary value", name);

    if (Rf_inherits(first_result, "factor")) {
      return FactorFlattener(data, name).get();
    }

    int rtype = TYPEOF(first_result);
    if (rtype == VECSXP) {
      return ListFlattener(data).get();
    }

    int n = data.size();
    for (int i = 1; i < n; i++) {
      rtype = rtype_promote(rtype, TYPEOF(data[i]));
    }
    if (rtype == VECSXP) return data;

    switch (rtype) {
    case LGLSXP:
      return Flattener<LGLSXP>(data).get();
    case RAWSXP:
      return Flattener<RAWSXP>(data).get();
    case INTSXP:
      return Flattener<INTSXP>(data).get();
    case REALSXP:
      return Flattener<REALSXP>(data).get();
    case CPLXSXP:
      return Flattener<CPLXSXP>(data).get();
    case STRSXP:
      return Flattener<STRSXP>(data).get();
    default:
      break;
    }

    stop("unknown result of type %d for column '%s'", TYPEOF(first_result), name.get_utf8_cstring());

    return data;
  }

  inline int rtype_promote(int x, int y) {
    // no promotion
    if (x == y) return x;

    // promote up
    if (valid_promotion_dispatch(x, y)) return y;
    if (valid_promotion_dispatch(y, x)) return x;

    // force promotion to VECSXP
    return VECSXP;
  }
};

template <>
inline SEXP GroupedCallReducer<NaturalDataFrame>::process(const NaturalDataFrame& gdf) {
  return process_chunk(NaturalSlicingIndex(gdf.nrows())) ;
}

}

#endif
