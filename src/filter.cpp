#include <dplyr.h>

#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/GroupedCallProxy.h>
#include <dplyr/Result/CallProxy.h>

using namespace Rcpp;
using namespace dplyr;

typedef dplyr_hash_set<SEXP> SymbolSet;

namespace dplyr {
  void strip_index(DataFrame x) {
    x.attr("indices") = R_NilValue;
    x.attr("group_sizes") = R_NilValue;
    x.attr("biggest_group_size") = R_NilValue;
    x.attr("labels") = R_NilValue;
  }
}

inline SEXP empty_subset(const DataFrame& df, CharacterVector columns, CharacterVector classes) {
  DataFrame res = DataFrameSubsetVisitors(df, columns).subset(EmptySubset(), classes);
  strip_index(res);
  return res;
}

SEXP assert_correct_filter_subcall(SEXP x, const SymbolSet& set, const Environment& env) {
  switch (TYPEOF(x)) {
  case LGLSXP:
    return x;
  case LANGSXP:
    return x;
  case SYMSXP:
  {
    if (set.count(x)) return x;

    // look in the environment
    SEXP var = PROTECT(Rf_findVar(x, env));
    SEXP res = Rf_duplicate(var);
    UNPROTECT(1);
    if (res == R_UnboundValue) {
      if (x == Rf_install("T")) {
        return Rf_ScalarLogical(TRUE);
      } else if (x == Rf_install("F")) {
        return Rf_ScalarLogical(FALSE);
      }
      stop("unknown column : %s", CHAR(PRINTNAME(x)));
    }
    return res;
  }
  default:
    break;
  }
  stop("incompatible expression in filter");
  return x; // never happens
}

SEXP and_calls(const LazyDots& dots, const SymbolSet& set, const Environment& env) {
  int ncalls = dots.size();
  if (!ncalls) {
    stop("incompatible input");
  }
  Shield<SEXP> call_(dots[0].expr());

  RObject res(assert_correct_filter_subcall(call_, set, env));

  SEXP and_symbol = Rf_install("&");
  for (int i=1; i<ncalls; i++) {
    Shield<SEXP> call(dots[i].expr());
    res = Rcpp_lang3(and_symbol, res, assert_correct_filter_subcall(call, set, env));
  }
  return res;
}

void check_filter_result(const LogicalVector& test, int n) {
  if (test.size() != n) {
    stop("incorrect length (%d), expecting: %d", test.size(), n);
  }
}

inline SEXP check_filter_logical_result(SEXP tmp) {
  if (TYPEOF(tmp) != LGLSXP) {
    stop("filter condition does not evaluate to a logical vector. ");
  }
  return tmp;
}

template <typename Data>
inline DataFrame grouped_subset(const Data& gdf, const LogicalVector& test, const CharacterVector& names, CharacterVector classes) {
  DataFrame data = gdf.data();
  DataFrame res = subset(data, test, names, classes);
  res.attr("vars")   = data.attr("vars");
  strip_index(res);
  return Data(res).data();
}

template <typename Data, typename Subsets>
DataFrame filter_grouped_single_env(const Data& gdf, const LazyDots& dots) {
  typedef GroupedCallProxy<Data, Subsets> Proxy;
  Environment env = dots[0].env();

  const DataFrame& data = gdf.data();
  CharacterVector names = data.names();
  SymbolSet set;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i]));
  }

  // a, b, c ->  a & b & c
  Call call(and_calls(dots, set, env));

  int nrows = data.nrows();
  LogicalVector test(nrows, TRUE);

  LogicalVector g_test;
  Proxy call_proxy(call, gdf, env);

  int ngroups = gdf.ngroups();
  typename Data::group_iterator git = gdf.group_begin();
  for (int i=0; i<ngroups; i++, ++git) {
    SlicingIndex indices = *git;
    int chunk_size = indices.size();

    g_test = check_filter_logical_result(call_proxy.get(indices));
    if (g_test.size() == 1) {
      int val = g_test[0] == TRUE;
      for (int j=0; j<chunk_size; j++) {
        test[ indices[j] ] = val;
      }
    } else {
      check_filter_result(g_test, chunk_size);
      for (int j=0; j<chunk_size; j++) {
        if (g_test[j] != TRUE) test[ indices[j] ] = FALSE;
      }
    }
  }
  return grouped_subset<Data>(gdf, test, names, classes_grouped<Data>());
}

// version of grouped filter when contributions to ... come from several environment
template <typename Data, typename Subsets>
DataFrame filter_grouped_multiple_env(const Data& gdf, const LazyDots& dots) {
  const DataFrame& data = gdf.data();
  CharacterVector names = data.names();
  SymbolSet set;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i]));
  }

  int nrows = data.nrows();
  LogicalVector test(nrows, TRUE);

  LogicalVector g_test;

  for (int k=0; k<dots.size(); k++) {
    Rcpp::checkUserInterrupt();
    const Lazy& lazy = dots[k];

    Call call(lazy.expr());
    GroupedCallProxy<Data, Subsets> call_proxy(call, gdf, lazy.env());
    int ngroups = gdf.ngroups();
    typename Data::group_iterator git = gdf.group_begin();
    for (int i=0; i<ngroups; i++, ++git) {
      SlicingIndex indices = *git;
      int chunk_size = indices.size();

      g_test  = check_filter_logical_result(call_proxy.get(indices));
      if (g_test.size() == 1) {
        if (g_test[0] != TRUE) {
          for (int j=0; j<chunk_size; j++) {
            test[indices[j]] = FALSE;
          }
        }
      } else {
        check_filter_result(g_test, chunk_size);
        for (int j=0; j<chunk_size; j++) {
          if (g_test[j] != TRUE) {
            test[ indices[j] ] = FALSE;
          }
        }
      }
    }
  }
  return grouped_subset<Data>(gdf, test, names, classes_grouped<Data>());
}

template <typename Data, typename Subsets>
DataFrame filter_grouped(const Data& gdf, const LazyDots& dots) {
  if (dots.single_env()) {
    return filter_grouped_single_env<Data, Subsets>(gdf, dots);
  } else {
    return filter_grouped_multiple_env<Data, Subsets>(gdf, dots);
  }
}

bool combine_and(LogicalVector& test, const LogicalVector& test2) {
  int n = test.size();
  if (n == 1) {
    test = test2;
  } else {
    int n2 = test2.size();
    if (n2 == 1) {
      if (!test2[0]) {
        return true;
      }
    } else if (n2 == n) {
      for (int i=0; i<n; i++) {
        test[i] = test[i] && test2[i];
      }
    } else {
      stop("incompatible sizes");
    }
  }
  return false;
}

DataFrame filter_not_grouped(DataFrame df, const LazyDots& dots) {
  CharacterVector names = df.names();
  SymbolSet set;
  for (int i=0; i<names.size(); i++) {
    set.insert(Rf_installChar(names[i]));
  }
  if (dots.single_env()) {
    Environment env = dots[0].env();
    // a, b, c ->  a & b & c
    Shield<SEXP> call(and_calls(dots, set, env));

    // replace the symbols that are in the data frame by vectors from the data frame
    // and evaluate the expression
    CallProxy proxy((SEXP)call, df, env);
    LogicalVector test = check_filter_logical_result(proxy.eval());

    if (test.size() == 1) {
      if (test[0] == TRUE) {
        return df;
      } else {
        return empty_subset(df, df.names(), classes_not_grouped());
      }
    } else {
      check_filter_result(test, df.nrows());
      return subset(df, test, classes_not_grouped());
    }
  } else {
    int nargs = dots.size();

    Call call(dots[0].expr());
    CallProxy first_proxy(call, df, dots[0].env());
    LogicalVector test = check_filter_logical_result(first_proxy.eval());
    if (test.size() == 1) {
      if (!test[0]) {
        return empty_subset(df, df.names(), classes_not_grouped());
      }
    } else {
      check_filter_result(test, df.nrows());
    }

    for (int i=1; i<nargs; i++) {
      Rcpp::checkUserInterrupt();

      Call call(dots[i].expr());
      CallProxy proxy(call, df, dots[i].env());
      LogicalVector test2 = check_filter_logical_result(proxy.eval());
      if (combine_and(test, test2)) {
        return empty_subset(df, df.names(), classes_not_grouped());
      }
    }

    DataFrame res = subset(df, test, classes_not_grouped());
    return res;
  }
}

// [[Rcpp::export]]
SEXP filter_impl(DataFrame df, LazyDots dots) {
  if (df.nrows() == 0 || Rf_isNull(df)) {
    return df;
  }
  check_valid_colnames(df);
  assert_all_white_list(df);

  if (dots.size() == 0) return df;

  // special case
  if (dots.size() == 1 && TYPEOF(dots[0].expr()) == LGLSXP) {
    LogicalVector what = dots[0].expr();
    if (what.size() == 1) {
      if (what[0] == TRUE) {
        return df;
      } else {
        return empty_subset(df, df.names(), is<GroupedDataFrame>(df) ? classes_grouped<GroupedDataFrame>() : classes_not_grouped());
      }
    }
  }
  if (is<GroupedDataFrame>(df)) {
    return filter_grouped<GroupedDataFrame, LazyGroupedSubsets>(GroupedDataFrame(df), dots);
  } else if (is<RowwiseDataFrame>(df)) {
    return filter_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(RowwiseDataFrame(df), dots);
  } else {
    return filter_not_grouped(df, dots);
  }
}
