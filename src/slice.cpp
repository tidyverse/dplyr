#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/NaturalDataFrame.h>

#include <dplyr/Result/GroupedCallProxy.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/tbl_cpp.h>

using namespace Rcpp;
using namespace dplyr;

inline SEXP check_filter_integer_result(SEXP tmp) {
  if (TYPEOF(tmp) != INTSXP && TYPEOF(tmp) != REALSXP && TYPEOF(tmp) != LGLSXP) {
    stop("slice condition does not evaluate to an integer or numeric vector. ");
  }
  return tmp;
}

class CountIndices {
public:
  CountIndices(int nr_, IntegerVector test_) : nr(nr_), test(test_), n_pos(0), n_neg(0) {

    for (int j = 0; j < test.size(); j++) {
      int i = test[j];
      if (i > 0 && i <= nr) {
        n_pos++;
      } else if (i < 0 && i >= -nr) {
        n_neg++;
      }
    }

    if (n_neg > 0 && n_pos > 0) {
      stop("Indices must be either all positive or all negative, not a mix of both. Found %d positive indices and %d negative indices", n_pos, n_neg);
    }

  }

  inline bool is_positive() const {
    return n_pos > 0;
  }
  inline int get_n_positive() const {
    return n_pos;
  }
  inline int get_n_negative() const {
    return n_neg;
  }

private:
  int nr;
  IntegerVector test;
  int n_pos;
  int n_neg;
};

template <typename SlicedTibble>
DataFrame slice_template(const SlicedTibble& gdf, const QuosureList& dots) {
  typedef LazySplitSubsets<SlicedTibble> LazySubsets;
  typedef GroupedCallProxy<SlicedTibble, LazySubsets> Proxy;
  typedef typename SlicedTibble::group_iterator group_iterator;
  typedef typename SlicedTibble::slicing_index Index ;
  typedef LazySplitSubsets<SlicedTibble> LazySubsets;

  const DataFrame& data = gdf.data();
  const NamedQuosure& quosure = dots[0];
  Environment env = quosure.env();
  SymbolVector names = data.names();

  // we already checked that we have only one expression
  Call call(quosure.expr());

  std::vector<int> indx;
  indx.reserve(1000);

  IntegerVector g_test;
  Proxy call_proxy(call, gdf, env);

  int ngroups = gdf.ngroups();
  group_iterator git = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++git) {
    const Index& indices = *git;
    int nr = indices.size();
    g_test = check_filter_integer_result(call_proxy.get(indices));
    CountIndices counter(indices.size(), g_test);

    if (counter.is_positive()) {
      // positive indexing
      int ntest = g_test.size();
      for (int j = 0; j < ntest; j++) {
        // only keep things inside inside 1:nr
        // this skips 0 and NA (which is negative (-2^31) for INTSXP)
        if (g_test[j] >= 1 && g_test[j] <= nr) {
          indx.push_back(indices[g_test[j] - 1]);
        }
      }
    } else if (counter.get_n_negative() != 0) {
      // negative indexing
      std::set<int> drop;
      int n = g_test.size();
      for (int j = 0; j < n; j++) {
        if (g_test[j] != NA_INTEGER && g_test[j] != 0)
          drop.insert(-g_test[j]);
      }
      int n_drop = drop.size();
      std::set<int>::const_iterator drop_it = drop.begin();

      int k = 0, j = 0;
      while (drop_it != drop.end()) {
        int next_drop = *drop_it - 1;
        while (j < next_drop) {
          indx.push_back(indices[j++]);
          k++;
        }
        j++;
        ++drop_it;
      }
      while (k < nr - n_drop) {
        indx.push_back(indices[j++]);
        k++;
      }
    }
  }

  DataFrame res = subset(data, indx, names, classes_not_grouped());
  copy_class(res, data) ;
  set_vars(res, get_vars(data));
  strip_index(res);

  return GroupedDataFrame(res).data();
}

// [[Rcpp::export]]
SEXP slice_impl(DataFrame df, QuosureList dots) {
  if (dots.size() == 0) return df;
  if (dots.size() != 1)
    stop("slice only accepts one expression");
  if (is<GroupedDataFrame>(df)) {
    return slice_template<GroupedDataFrame>(GroupedDataFrame(df), dots);
  } else {
    return slice_template<NaturalDataFrame>(NaturalDataFrame(df), dots);
  }
}
