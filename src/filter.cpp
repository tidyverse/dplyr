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

// class to collect indices for each group
template <typename Index>
class GroupFilterIndices {
public:
  int ngroups;

  // the old indices
  std::vector<Index> old_indices;

  // the results of the test expression for each group
  // we only keep those that we need
  Rcpp::List tests;

  // The new indices
  Rcpp::List new_indices;

  // The group sizes
  Rcpp::IntegerVector group_sizes;

  // size of the biggest group
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

  // set the group i to be empty
  void empty_group(int i) {
    group_sizes[i] = 0;
    new_indices[i] = Rcpp::IntegerVector::create();
  }

  // the group i contains all the data from the original
  void add_dense_group(int i, const Index& old_idx, int n) {
    add_group(i, old_idx, n);
  }

  // the group i contains some data, available in g_test
  void add_group_lgl(int i, const Index& old_idx, int n, Rcpp::LogicalVector g_test) {
    if (n == 0) {
      empty_group(i);
    } else {
      add_group(i, old_idx, n) ;
      tests[i] = g_test;
    }
  }

  // the total number of rows
  // only makes sense when the object is fully trained
  inline int size() const {
    return k;
  }

  // is the group i dense
  inline bool is_dense(int i) const {
    return group_sizes[i] == old_indices[i].size();
  }

private:

  void add_group(int i, const Index& old_idx, int n) {
    old_indices[i] = old_idx;
    group_sizes[i] = n;
    new_indices[i] = Rcpp::seq(k, k + n - 1);
    if (biggest_group_size < n) biggest_group_size = n;
    k += n ;
  }

};

template <int RTYPE>
class FilterVector {
public:
  typedef Rcpp::Vector<RTYPE> type ;

  FilterVector(int n, const type& source_) :
    source(source_), data(no_init(n))
  {
    copy_most_attributes(data, source);
  }

  inline void copy(int i, int j){
    data[i] = source[j];
  }

  inline operator SEXP(){
    return data;
  }

private:
  const type& source ;
  type data ;
};

template <int RTYPE>
class FilterMatrix {
public:
  typedef Rcpp::Matrix<RTYPE> type ;

  FilterMatrix(int n, const type& source_) :
    source(source_), data(n, source.ncol())
  {
    copy_most_attributes(data, source);
  }

  inline void copy(int i, int j){
    data.row(i) = source.row(j);
  }

  inline operator SEXP(){
    return data;
  }

private:
  const type& source ;
  type data ;
};


// subset a vector using indices collected in an Index
template <int RTYPE, typename Index, template<int> class Data>
class FilterVisitor {
public:
  typedef typename Data<RTYPE>::type data_type;

  FilterVisitor(data_type data_) :
    data(data_)
  {}

  virtual SEXP subset(const GroupFilterIndices<Index>& idx) {
    int n = idx.size();
    Data<RTYPE> out(n, data);

    for (int i = 0; i < idx.ngroups; i++) {
      int group_size = idx.group_sizes[i];
      // because there is nothing to do when the group is empty
      if (group_size > 0) {
        // the indices relevant to the original data
        const Index& old_idx = idx.old_indices[i];

        // the new indices
        IntegerVector new_idx = idx.new_indices[i];
        if (idx.is_dense(i)) {
          // in that case we can just all the data
          for (int j = 0; j < group_size; j++) {
            out.copy(new_idx[j], old_idx[j]);
          }
        } else {
          // otherwise we copy only the data for which test is TRUE
          // so we discard FALSE and NA
          LogicalVector test = idx.tests[i];
          for (int j = 0, k = 0; j < group_size; j++, k++) {
            while (test[k] != TRUE) k++ ;
            out.copy(new_idx[j], old_idx[k]);
          }
        }
      }
    }
    return out;
  }

private:
  data_type data;
};

template <typename Index, template<int> class Container>
inline SEXP filter_visit_impl(SEXP data, const GroupFilterIndices<Index>& idx) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return FilterVisitor<INTSXP, Index, Container>(data).subset(idx);
  case REALSXP:
    return FilterVisitor<REALSXP, Index, Container>(data).subset(idx);
  case LGLSXP:
    return FilterVisitor<LGLSXP, Index, Container>(data).subset(idx);
  case STRSXP:
    return FilterVisitor<STRSXP, Index, Container>(data).subset(idx);
  case RAWSXP:
    return FilterVisitor<RAWSXP, Index, Container>(data).subset(idx);
  case CPLXSXP:
    return FilterVisitor<CPLXSXP, Index, Container>(data).subset(idx);
  case VECSXP:
    return FilterVisitor<VECSXP, Index, Container>(data).subset(idx);
  }

  return R_NilValue;
}

// subset `data` with indices collected in `idx`
template <typename Index>
inline SEXP filter_visit(SEXP data, const GroupFilterIndices<Index>& idx) {
  if (Rf_isMatrix(data)) {
    return filter_visit_impl<Index, FilterVector>(data, idx) ;
  } else {
    return filter_visit_impl<Index, FilterMatrix>(data, idx) ;
  }
}

// template class to rebuild the attributes
// in the general case there is nothing to do
template <typename Index, typename SlicedTibble>
class SlicedTibbleRebuilder {
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<Index>& index, const DataFrame& data) {}
  void reconstruct(List& out) {}
};

// specific case for GroupedDataFrame
// we need to take care of the attributes `indices`, `labels`, `vars`, `group_sizes`, `biggest_group_size`
template <typename Index>
class SlicedTibbleRebuilder<Index, GroupedDataFrame> {
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<Index>& index_, const DataFrame& data_) :
    index(index_),
    data(data_)
  {}

  void reconstruct(List& out) {
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
SEXP filter_template(const SlicedTibble& gdf, const NamedQuosure& quo) {
  typedef LazySplitSubsets<SlicedTibble> LazySubsets;
  typedef GroupedCallProxy<SlicedTibble, LazySubsets> Proxy;
  typedef typename SlicedTibble::group_iterator GroupIterator;
  typedef typename SlicedTibble::slicing_index Index ;

  Proxy call_proxy(quo.expr(), gdf, quo.env()) ;
  GroupIterator git = gdf.group_begin();

  const DataFrame& data = gdf.data() ;
  int ngroups = gdf.ngroups() ;

  // tracking the indices for each group
  GroupFilterIndices<Index> group_indices(ngroups);

  // traverse each group and fill `group_indices`
  for (int i = 0; i < ngroups; i++, ++git) {
    const Index& indices = *git;
    int chunk_size = indices.size();

    // empty group size. no need to evaluate the expression
    if (chunk_size == 0) {
      group_indices.empty_group(i) ;
      continue;
    }

    // the result of the expression in the group
    LogicalVector g_test = check_result_lgl_type(call_proxy.get(indices));
    if (g_test.size() == 1) {
      // we get length 1 so either we have an empty group, or a dense group, i.e.
      // a group that has all the rows from the original data
      if (g_test[0] == TRUE) {
        group_indices.add_dense_group(i, indices, chunk_size) ;
      } else {
        group_indices.empty_group(i);
      }
    } else {
      // any other size, so we check that it is consistent with the group size
      check_result_length(g_test, chunk_size);
      int yes = std::count(g_test.begin(), g_test.end(), TRUE);
      group_indices.add_group_lgl(i, indices, yes, g_test);
    }
  }


  // create the result data frame
  int nc = data.size();
  List out(data.size());

  // this is shared by all types of SlicedTibble
  copy_most_attributes(out, data);
  copy_class(out, data);
  copy_names(out, data);
  set_rownames(out, group_indices.size());

  // extract each column
  for (int i = 0; i < nc; i++) {
    out[i] = filter_visit(data[i], group_indices);
  }

  // set the specific attributes
  // currently this only does anything for SlicedTibble = GroupedDataFrame
  // i.e. retain the indices, labels, ... attributes
  SlicedTibbleRebuilder<Index, SlicedTibble>(group_indices, data).reconstruct(out);

  return out;
}

// [[Rcpp::export]]
SEXP filter_impl(DataFrame df, NamedQuosure quo) {
  if (df.nrows() == 0 || Rf_isNull(df)) {
    return df;
  }
  check_valid_colnames(df);
  assert_all_white_list(df);

  if (is<GroupedDataFrame>(df)) {
    return filter_template<GroupedDataFrame>(GroupedDataFrame(df), quo);
  } else if (is<RowwiseDataFrame>(df)) {
    return filter_template<RowwiseDataFrame>(RowwiseDataFrame(df), quo);
  } else {
    return filter_template<NaturalDataFrame>(NaturalDataFrame(df), quo);
  }
}
