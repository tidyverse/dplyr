#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/Quosure.h>
#include <tools/utils.h>
#include <tools/SymbolString.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/NaturalDataFrame.h>

#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/GroupedCallProxy.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/bad.h>
#include <dplyr/tbl_cpp.h>

using namespace Rcpp;
using namespace dplyr;

inline
SEXP empty_subset(const DataFrame& df, const CharacterVector& classes) {
  DataFrame res = DataFrameSubsetVisitors(df).subset(EmptySubset(), classes);
  strip_index(res);
  return res;
}

inline
void check_result_length(const LogicalVector& test, int n) {
  if (test.size() != n) {
    stop("Result must have length %d, not %d", n, test.size());
  }
}

inline
SEXP check_result_lgl_type(SEXP tmp) {
  if (TYPEOF(tmp) != LGLSXP) {
    bad_pos_arg(2, "filter condition does not evaluate to a logical vector");
  }
  return tmp;
}

template <typename Index>
class GroupFilterIndices {
public:
  int ngroups;

  std::vector<Index> old_indices;
  Rcpp::List tests;
  Rcpp::List new_indices;
  Rcpp::IntegerVector group_sizes;
  int biggest_group_size ;

private:

  int k;

public:

  GroupFilterIndices(int ngroups_) :
  ngroups(ngroups_),

  old_indices(ngroups),
  tests(ngroups),
  new_indices(ngroups),
  group_sizes(ngroups),
  biggest_group_size(0),
  k(0)
  {}

  void empty_group(int i){
    group_sizes[i] = 0;
    new_indices[i] = Rcpp::IntegerVector::create();
  }

  void add_group(int i, const Index& old_idx, int n) {
    old_indices[i] = old_idx;
    group_sizes[i] = n;
    new_indices[i] = Rcpp::seq(k, k+n-1) ;
    if (biggest_group_size < n) biggest_group_size = n;
    k += n ;
  }

  void add_group_lgl(int i, const Index& old_idx, int n, Rcpp::LogicalVector g_test){
    add_group(i, old_idx, n) ;
    tests[i] = g_test;
  }

  inline int size() const {
    return k;
  }

  inline bool is_full(int i) const {
    return group_sizes[i] == old_indices[i].size();
  }

};


template <typename Index>
class FilterVectorVisitor {
public:
  virtual ~FilterVectorVisitor() {};

  virtual SEXP subset(const GroupFilterIndices<Index>&) = 0;
};

template <int RTYPE, typename Index>
class FilterVectorVisitorImpl : public FilterVectorVisitor<Index> {
public:
  typedef typename Rcpp::Vector<RTYPE> Vec;

  FilterVectorVisitorImpl(Vec data_) :
    data(data_)
  {}

  virtual SEXP subset(const GroupFilterIndices<Index>& idx) {
    int n = idx.size();
    Vec out = Rcpp::no_init(n);
    copy_most_attributes(out, data);

    for(int i=0; i<idx.ngroups; i++){
      int group_size = idx.group_sizes[i];
      if (group_size > 0) {
        const Index& old_idx = idx.old_indices[i];

        IntegerVector new_idx = idx.new_indices[i];
        if (idx.is_full(i)) {
          for (int j=0; j<group_size; j++) {
            out[new_idx[j]] = data[old_idx[j]];
          }
        } else {
          LogicalVector test = idx.tests[i];
          for (int j=0, k=0; j<group_size; j++) {
            while(test[k] != TRUE) k++ ;
            out[new_idx[j]] = data[old_idx[k]];
          }
        }
      }
    }
    return out;
  }

private:
  Vec data;
};

template <typename Index>
inline SEXP filter_visit(SEXP data, const GroupFilterIndices<Index>& idx) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return FilterVectorVisitorImpl<INTSXP, Index>(data).subset(idx);
  case REALSXP:
    return FilterVectorVisitorImpl<REALSXP, Index>(data).subset(idx);
  case LGLSXP:
    return FilterVectorVisitorImpl<LGLSXP, Index>(data).subset(idx);
  case STRSXP:
    return FilterVectorVisitorImpl<STRSXP, Index>(data).subset(idx);
  case RAWSXP:
    return FilterVectorVisitorImpl<RAWSXP, Index>(data).subset(idx);
  case CPLXSXP:
    return FilterVectorVisitorImpl<CPLXSXP, Index>(data).subset(idx);
  }

  return R_NilValue;
}

template <typename Index, typename SlicedTibble>
class SlicedTibbleRebuilder{
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<Index>& index, const DataFrame& data){}
  void reconstruct(List& out){}
};

template <typename Index>
class SlicedTibbleRebuilder<Index, GroupedDataFrame>{
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<Index>& index_, const DataFrame& data_) :
    index(index_),
    data(data_)
  {}

  void reconstruct(List& out){
    out.attr("indices") = index.new_indices;
    out.attr("vars") = data.attr("vars");
    out.attr("group_sizes") = index.group_sizes;
    out.attr("labels") = data.attr("labels");
    out.attr("biggest_group_size") = index.biggest_group_size;
  }

private:
  const GroupFilterIndices<Index>& index;
  const DataFrame& data;
};


template <typename SlicedTibble>
SEXP filter_impl(const SlicedTibble& gdf, const NamedQuosure& quo) {
  typedef LazySplitSubsets<SlicedTibble> LazySubsets;
  typedef GroupedCallProxy<SlicedTibble, LazySubsets> Proxy;
  typedef typename SlicedTibble::group_iterator GroupIterator;
  typedef typename SlicedTibble::slicing_index Index ;

  Proxy call_proxy(quo.expr(), gdf, quo.env()) ;
  GroupIterator git = gdf.group_begin();

  int ngroups = gdf.ngroups() ;
  GroupFilterIndices<Index> group_indices(ngroups);

  for (int i = 0; i < ngroups; i++, ++git) {
    const Index& indices = *git;
    int chunk_size = indices.size();

    // special case with empty group size. no need to evaluate the expression
    if (chunk_size == 0) {
      group_indices.empty_group(i) ;
      continue;
    }

    LogicalVector g_test = check_result_lgl_type(call_proxy.get(indices));
    if (g_test.size() == 1) {
      // recycle
      if (g_test[0] == TRUE){
        group_indices.add_group(i, indices, chunk_size) ;
      } else {
        group_indices.empty_group(i);
      }
    } else {
      check_result_length(g_test, chunk_size);
      int yes = std::count(g_test.begin(), g_test.end(), TRUE);
      group_indices.add_group_lgl(i, indices, yes, g_test);
    }
  }

  const DataFrame& data = gdf.data() ;

  int nc = data.size();
  List out(data.size());

  // this is shared by all types of SlicedTibble
  copy_most_attributes(out, data);
  copy_class(out, data);
  copy_names(out, data);
  set_rownames(out, group_indices.size());

  // extract each column
  for (int i=0; i<nc; i++) {
    out[i] = filter_visit(data[i], group_indices);
  }

  // set the specific attributes
  SlicedTibbleRebuilder<Index,SlicedTibble>(group_indices, data).reconstruct(out);

  return out;
}

// DataFrame filter_ungrouped(DataFrame df, const NamedQuosure& quo) {
//   CallProxy proxy(quo.expr(), df, quo.env());
//   LogicalVector test = check_result_lgl_type(proxy.eval());
//
//   if (test.size() == 1) {
//     if (test[0] == TRUE) {
//       return df;
//     } else {
//       return empty_subset(df, classes_not_grouped());
//     }
//   } else {
//     check_result_length(test, df.nrows());
//     return subset(df, test, classes_not_grouped());
//   }
// }

// [[Rcpp::export]]
SEXP filter_impl(DataFrame df, NamedQuosure quo) {
  if (df.nrows() == 0 || Rf_isNull(df)) {
    return df;
  }
  check_valid_colnames(df);
  assert_all_white_list(df);

  if (is<GroupedDataFrame>(df)) {
    return filter_impl<GroupedDataFrame>(GroupedDataFrame(df), quo);
  } else if (is<RowwiseDataFrame>(df)) {
    return filter_impl<RowwiseDataFrame>(RowwiseDataFrame(df), quo);
  } else {
    return filter_impl<NaturalDataFrame>(NaturalDataFrame(df), quo);
  }
}
