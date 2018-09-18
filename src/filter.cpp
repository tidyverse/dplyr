#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/Quosure.h>
#include <tools/utils.h>
#include <tools/SymbolString.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>
#include <dplyr/data/DataMask.h>

#include <tools/bad.h>
#include <tools/set_rownames.h>

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

inline SEXP check_filter_integer_result(SEXP tmp) {
  if (TYPEOF(tmp) != INTSXP && TYPEOF(tmp) != REALSXP && TYPEOF(tmp) != LGLSXP) {
    stop("slice condition does not evaluate to an integer or numeric vector. ");
  }
  return tmp;
}

struct SlicePositivePredicate {
  int max;
  SlicePositivePredicate(int max_) : max(max_) {}

  inline bool operator()(int i) const {
    return i > 0 && i <= max ;
  }
};

struct SliceNegativePredicate {
  int min;
  SliceNegativePredicate(int max_) : min(-max_) {}

  inline bool operator()(int i) const {
    return i >= min && i < 0;
  }
};

// class to collect indices for each group
template <typename SlicedTibble>
class GroupFilterIndices {
public:
  typedef typename SlicedTibble::slicing_index slicing_index;
  int ngroups;

  // the old indices
  std::vector<slicing_index> old_indices;

  // the results of the test expression for each group
  // we only keep those that we need
  Rcpp::List tests;

  // The new indices
  Rcpp::List new_indices;

  // dense
  std::vector<bool> dense;

private:

  int k;

public:

  GroupFilterIndices(int ngroups_) :
    ngroups(ngroups_),

    old_indices(ngroups),
    tests(ngroups),
    new_indices(ngroups),
    dense(ngroups, false),
    k(0)
  {}

  // set the group i to be empty
  void empty_group(int i) {
    new_indices[i] = Rcpp::IntegerVector::create();
  }

  // the group i contains all the data from the original
  void add_dense_group(int i, const slicing_index& old_idx, int n) {
    add_group(i, old_idx, n);
    dense[i] = true;
  }

  // the group i contains some data, available in g_test
  void add_group_lgl(int i, const slicing_index& old_idx, int n, Rcpp::LogicalVector g_test) {
    if (n == 0) {
      empty_group(i);
    } else {
      add_group(i, old_idx, n) ;
      tests[i] = g_test;
    }
  }

  void add_group_slice_positive(int i, const slicing_index& old_idx, const IntegerVector& g_idx) {
    int old_group_size = old_idx.size();
    int new_group_size = std::count_if(g_idx.begin(), g_idx.end(), SlicePositivePredicate(old_group_size));
    if (new_group_size == 0) {
      empty_group(i);
    } else {
      add_group(i, old_idx, new_group_size);
      tests[i] = g_idx ;
    }
  }

  void add_group_slice_negative(int i, const slicing_index& old_idx, const IntegerVector& g_idx) {
    int old_group_size = old_idx.size();
    SliceNegativePredicate pred(old_group_size);
    LogicalVector test(old_group_size, TRUE);
    for (int j = 0; j < g_idx.size(); j++) {
      int idx = g_idx[j];
      if (pred(idx)) {
        test[-idx - 1] = FALSE;
      }
    }
    int n = std::count(test.begin(), test.end(), TRUE);
    add_group_lgl(i, old_idx, n, test);
  }


  // the total number of rows
  // only makes sense when the object is fully trained
  inline int size() const {
    return k;
  }

  inline int group_size(int i) const {
    return Rf_length(new_indices[i]);
  }

  // is the group i dense
  inline bool is_dense(int i) const {
    return dense[i];
  }

private:

  void add_group(int i, const slicing_index& old_idx, int n) {
    old_indices[i] = old_idx;
    new_indices[i] = Rcpp::seq(k + 1, k + n);
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

  inline void copy(int i, int j) {
    data[i] = source[j];
  }

  inline operator SEXP() {
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

  inline void copy(int i, int j) {
    data.row(i) = source.row(j);
  }

  inline operator SEXP() {
    return data;
  }

private:
  const type& source ;
  type data ;
};


// subset a vector using indices collected in an Index
template <int RTYPE, typename SlicedTibble, template<int> class Data>
class FilterVisitor {
public:
  typedef typename SlicedTibble::slicing_index slicing_index;
  typedef typename Data<RTYPE>::type data_type;

  FilterVisitor(data_type data_) :
    data(data_)
  {}

  virtual SEXP subset(const GroupFilterIndices<SlicedTibble>& idx) {
    int n = idx.size();
    Data<RTYPE> out(n, data);

    for (int i = 0; i < idx.ngroups; i++) {
      int group_size = idx.group_size(i);
      // because there is nothing to do when the group is empty
      if (group_size > 0) {
        // the indices relevant to the original data
        const slicing_index& old_idx = idx.old_indices[i];

        // the new indices
        IntegerVector new_idx = idx.new_indices[i];
        if (idx.is_dense(i)) {
          // in that case we can just all the data
          for (int j = 0; j < group_size; j++) {
            out.copy(new_idx[j] - 1, old_idx[j]);
          }
        } else {
          SEXP test = idx.tests[i];

          if (is<LogicalVector>(test)) {
            copy_all_lgl(test, out, group_size, new_idx, old_idx);
          } else {
            copy_all_int(test, out, group_size, new_idx, old_idx);
          }
        }
      }
    }
    return out;
  }

private:

  void copy_all_lgl(LogicalVector test, Data<RTYPE>& out, int group_size, const IntegerVector& new_idx, const slicing_index& old_idx) {
    for (int j = 0, k = 0; j < group_size; j++, k++) {
      while (test[k] != TRUE) k++ ;
      out.copy(new_idx[j] - 1, old_idx[k]);
    }
  }

  void copy_all_int(IntegerVector test, Data<RTYPE>& out, int group_size, const IntegerVector& new_idx, const slicing_index& old_idx) {
    SlicePositivePredicate pred(old_idx.size());
    for (int j = 0, k = 0; j < group_size; j++, k++) {
      while (!pred(test[k])) k++ ;
      out.copy(new_idx[j] - 1, old_idx[test[k] - 1]);
    }
  }

  data_type data;
};

template <typename SlicedTibble, template<int> class Container>
inline SEXP filter_visit_impl(SEXP data, const GroupFilterIndices<SlicedTibble>& idx) {
  switch (TYPEOF(data)) {
  case INTSXP:
    return FilterVisitor<INTSXP, SlicedTibble, Container>(data).subset(idx);
  case REALSXP:
    return FilterVisitor<REALSXP, SlicedTibble, Container>(data).subset(idx);
  case LGLSXP:
    return FilterVisitor<LGLSXP, SlicedTibble, Container>(data).subset(idx);
  case STRSXP:
    return FilterVisitor<STRSXP, SlicedTibble, Container>(data).subset(idx);
  case RAWSXP:
    return FilterVisitor<RAWSXP, SlicedTibble, Container>(data).subset(idx);
  case CPLXSXP:
    return FilterVisitor<CPLXSXP, SlicedTibble, Container>(data).subset(idx);
  case VECSXP:
    return FilterVisitor<VECSXP, SlicedTibble, Container>(data).subset(idx);
  }

  return R_NilValue;
}

// subset `data` with indices collected in `idx`
template <typename SlicedTibble>
inline SEXP filter_visit(SEXP data, const GroupFilterIndices<SlicedTibble>& idx) {
  if (Rf_isMatrix(data)) {
    return filter_visit_impl<SlicedTibble, FilterMatrix>(data, idx) ;
  } else {
    return filter_visit_impl<SlicedTibble, FilterVector>(data, idx) ;
  }
}

// template class to rebuild the attributes
// in the general case there is nothing to do
template <typename SlicedTibble>
class SlicedTibbleRebuilder {
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<SlicedTibble>& index, const SlicedTibble& data) {}
  void reconstruct(List& out) {}
};

// specific case for GroupedDataFrame, we need to take care of `groups`
template <>
class SlicedTibbleRebuilder<GroupedDataFrame> {
public:
  SlicedTibbleRebuilder(const GroupFilterIndices<GroupedDataFrame>& index_, const GroupedDataFrame& data_) :
    index(index_),
    data(data_)
  {}

  void reconstruct(List& out) {
    DataFrame groups = update_groups(data.group_data(), index.new_indices);
    GroupedDataFrame::set_groups(out, groups);
  }

  SEXP update_groups(DataFrame old, List indices) {
    int nc = old.size();
    List groups(nc);
    copy_most_attributes(groups, old);
    copy_names(groups, old);

    // labels
    for (int i = 0; i < nc - 1; i++) groups[i] = old[i];

    // indices
    groups[nc - 1] = indices;

    return groups;
  }

private:
  const GroupFilterIndices<GroupedDataFrame>& index;
  const GroupedDataFrame& data;
};

template <typename SlicedTibble>
SEXP structure_filter(const SlicedTibble& gdf, const GroupFilterIndices<SlicedTibble>& group_indices) {
  const DataFrame& data = gdf.data();
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
  SlicedTibbleRebuilder<SlicedTibble>(group_indices, gdf).reconstruct(out);

  return out;
}


template <typename SlicedTibble>
SEXP filter_template(const SlicedTibble& gdf, const Quosure& quo) {
  typedef typename SlicedTibble::group_iterator GroupIterator;
  typedef typename SlicedTibble::slicing_index slicing_index;

  // Proxy call_proxy(quo.expr(), gdf, quo.env()) ;
  GroupIterator git = gdf.group_begin();
  DataMask<SlicedTibble> mask(gdf) ;
  mask.rechain(quo.env());

  int ngroups = gdf.ngroups() ;

  // tracking the indices for each group
  GroupFilterIndices<SlicedTibble> group_indices(ngroups);

  // traverse each group and fill `group_indices`
  for (int i = 0; i < ngroups; i++, ++git) {
    const slicing_index& indices = *git;
    int chunk_size = indices.size();

    // empty group size. no need to evaluate the expression
    if (chunk_size == 0) {
      group_indices.empty_group(i) ;
      continue;
    }

    // the result of the expression in the group
    LogicalVector g_test = check_result_lgl_type(mask.eval(quo.expr(), indices));
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

  return structure_filter<SlicedTibble>(gdf, group_indices) ;
}

// [[Rcpp::export]]
SEXP filter_impl(DataFrame df, Quosure quo) {
  if (df.nrows() == 0 || Rf_isNull(df)) {
    return df;
  }
  check_valid_colnames(df);
  assert_all_allow_list(df);

  if (is<GroupedDataFrame>(df)) {
    return filter_template<GroupedDataFrame>(GroupedDataFrame(df), quo);
  } else if (is<RowwiseDataFrame>(df)) {
    return filter_template<RowwiseDataFrame>(RowwiseDataFrame(df), quo);
  } else {
    return filter_template<NaturalDataFrame>(NaturalDataFrame(df), quo);
  }
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

  inline bool is_negative() const {
    return n_neg > 0;
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
DataFrame slice_template(const SlicedTibble& gdf, const Quosure& quo) {
  typedef typename SlicedTibble::group_iterator group_iterator;
  typedef typename SlicedTibble::slicing_index slicing_index ;

  DataMask<SlicedTibble> mask(gdf);
  mask.rechain(quo.env());

  const DataFrame& data = gdf.data() ;
  int ngroups = gdf.ngroups() ;
  SymbolVector names = data.names();

  GroupFilterIndices<SlicedTibble> group_indices(ngroups);

  group_iterator git = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++git) {
    const slicing_index& indices = *git;
    IntegerVector g_test = check_filter_integer_result(mask.eval(quo.expr(), indices));
    CountIndices counter(indices.size(), g_test);

    if (counter.is_positive()) {
      group_indices.add_group_slice_positive(i, indices, g_test);
    } else if (counter.is_negative()) {
      group_indices.add_group_slice_negative(i, indices, g_test);
    } else {
      group_indices.empty_group(i);
    }
  }

  return structure_filter<SlicedTibble>(gdf, group_indices);
}

// [[Rcpp::export]]
SEXP slice_impl(DataFrame df, Quosure quosure) {
  if (is<GroupedDataFrame>(df)) {
    return slice_template<GroupedDataFrame>(GroupedDataFrame(df), quosure);
  } else {
    return slice_template<NaturalDataFrame>(NaturalDataFrame(df), quosure);
  }
}
