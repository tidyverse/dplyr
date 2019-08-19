#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>
#include <tools/Quosure.h>
#include <tools/utils.h>
#include <tools/SymbolString.h>
#include <tools/bad.h>
#include <tools/set_rownames.h>
#include <tools/all_na.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>
#include <dplyr/data/DataMask.h>

namespace dplyr {

// template class to rebuild the attributes
// in the general case there is nothing to do
template <typename SlicedTibble, typename IndexCollector>
class FilterTibbleRebuilder {
public:
  FilterTibbleRebuilder(const IndexCollector& index, const SlicedTibble& data) {}
  void reconstruct(Rcpp::List& out) {}
};

// specific case for GroupedDataFrame, we need to take care of `groups`
template <typename IndexCollector>
class FilterTibbleRebuilder<GroupedDataFrame, IndexCollector> {
public:
  FilterTibbleRebuilder(const IndexCollector& index_, const GroupedDataFrame& data_) :
    index(index_),
    data(data_)
  {}

  void reconstruct(Rcpp::List& out) {
    GroupedDataFrame::set_groups(out, update_groups(data.group_data(), index.rows));
  }

private:

  SEXP update_groups(Rcpp::DataFrame old, Rcpp::List indices) {
    int nc = old.size();
    Rcpp::List groups(nc);
    copy_most_attributes(groups, old);
    copy_names(groups, old);

    // labels
    for (int i = 0; i < nc - 1; i++) groups[i] = old[i];

    // indices
    groups[nc - 1] = indices;

    return groups;
  }

  const IndexCollector& index;
  const GroupedDataFrame& data;
};

template <typename SlicedTibble, typename IndexCollector>
SEXP structure_filter(const SlicedTibble& gdf, const IndexCollector& group_indices, SEXP frame) {
  const Rcpp::DataFrame& data = gdf.data();
  // create the result data frame
  int nc = data.size();
  Rcpp::List out(nc);

  // this is shared by all types of SlicedTibble
  copy_most_attributes(out, data);
  copy_class(out, data);
  copy_names(out, data);
  set_rownames(out, group_indices.size());

  // retrieve the 1-based indices vector
  const Rcpp::IntegerVector& idx = group_indices.indices;

  // extract each column with column_subset
  for (int i = 0; i < nc; i++) {
    out[i] = column_subset(data[i], idx, frame);
  }

  // set the specific attributes
  // currently this only does anything for SlicedTibble = GroupedDataFrame
  FilterTibbleRebuilder<SlicedTibble, IndexCollector>(group_indices, gdf).reconstruct(out);

  return out;
}

}

// [[Rcpp::export(rng = false)]]
SEXP filter_update_rows(int n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes) {
  R_xlen_t n_groups = XLENGTH(new_rows_sizes);

  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n_groups));
  Rf_setAttrib(new_rows, R_ClassSymbol, dplyr::vectors::classes_vctrs_list_of);
  Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

  // allocate each new_rows element
  int* p_new_rows_sizes = INTEGER(new_rows_sizes);
  std::vector<int> tracks(n_groups);
  std::vector<int*> p_new_rows(n_groups);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP new_rows_i = Rf_allocVector(INTSXP, p_new_rows_sizes[i]);
    SET_VECTOR_ELT(new_rows, i, new_rows_i);
    p_new_rows[i] = INTEGER(new_rows_i);
  }

  // traverse group_indices and keep to fill new_rows
  int* p_group_indices = INTEGER(group_indices);
  int* p_keep = LOGICAL(keep);
  int j = 1;
  for (R_xlen_t i = 0; i < n_rows; i++) {
    if (p_keep[i] == TRUE) {
      int g = p_group_indices[i];
      int track = tracks[g - 1]++;
      p_new_rows[g - 1][track] = j++;
    }
  }

  UNPROTECT(1);

  return new_rows;
}

// ------------------------------------------------- slice()

namespace dplyr {

inline bool all_lgl_na(SEXP lgl) {
  R_xlen_t n = XLENGTH(lgl);
  int* p = LOGICAL(lgl);
  for (R_xlen_t i = 0; i < n; i++) {
    if (*p != NA_LOGICAL) {
      return false;
    }
  }
  return true;
}

inline void check_slice_result(SEXP tmp) {
  switch (TYPEOF(tmp)) {
  case INTSXP:
  case REALSXP:
    break;
  case LGLSXP:
    if (all_lgl_na(tmp)) break;
  default:
    Rcpp::stop("slice condition does not evaluate to an integer or numeric vector. ");
  }
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
class CountIndices {
public:
  CountIndices(int nr_, Rcpp::IntegerVector test_) : nr(nr_), test(test_), n_pos(0), n_neg(0) {

    for (int j = 0; j < test.size(); j++) {
      int i = test[j];
      if (i > 0 && i <= nr) {
        n_pos++;
      } else if (i < 0 && i >= -nr) {
        n_neg++;
      }
    }

    if (n_neg > 0 && n_pos > 0) {
      Rcpp::stop("Indices must be either all positive or all negative, not a mix of both. Found %d positive indices and %d negative indices", n_pos, n_neg);
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
  Rcpp::IntegerVector test;
  int n_pos;
  int n_neg;
};

template <typename SlicedTibble>
class GroupSliceIndices {
  typedef typename SlicedTibble::slicing_index slicing_index;

  const SlicedTibble& tbl;

  int n;

  std::vector<int> slice_indices;
  int k;

  int ngroups;

  std::vector<int> new_sizes;

  typename SlicedTibble::group_iterator git;

public:

  Rcpp::IntegerVector indices;
  Rcpp::List rows;

  GroupSliceIndices(const SlicedTibble& tbl_) :
    tbl(tbl_),
    n(tbl.data().nrow()),

    slice_indices(),
    k(0),

    ngroups(tbl.ngroups()),
    git(tbl.group_begin()),
    rows(ngroups)
  {
    // reserve enough space for positions and groups for most cases
    // i.e. in most cases we need less than n
    slice_indices.reserve(n);
  }

  // set the group i to be empty
  void empty_group(int i) {
    rows[i] = Rf_allocVector(INTSXP, 0);
    ++git;
  }

  void add_group_slice_positive(int i, const Rcpp::IntegerVector& g_idx) {
    slicing_index old_indices = *git;
    int ng = g_idx.size();
    SlicePositivePredicate pred(old_indices.size());
    int old_k = k;
    for (int j = 0; j < ng; j++) {
      if (pred(g_idx[j])) {
        slice_indices.push_back(old_indices[g_idx[j] - 1] + 1);
        k++;
      }
    }
    if (old_k == k) {
      rows[i] = Rf_allocVector(INTSXP, 0);
    } else {
      rows[i] = Rcpp::IntegerVectorView(Rcpp::seq(old_k + 1, k));
    }
    ++git;
  }

  void add_group_slice_negative(int i, const Rcpp::IntegerVector& g_idx) {
    slicing_index old_indices = *git;
    SliceNegativePredicate pred(old_indices.size());

    Rcpp::LogicalVector test_lgl(old_indices.size(), TRUE);
    for (int j = 0; j < g_idx.size(); j++) {
      int idx = g_idx[j];
      if (pred(idx)) {
        test_lgl[-idx - 1] = FALSE;
      }
    }
    int ng = std::count(test_lgl.begin(), test_lgl.end(), TRUE);

    if (ng == 0) {
      empty_group(i);
    } else {
      int old_k = k;
      Rcpp::IntegerVector test(ng);
      for (int j = 0; j < test_lgl.size(); j++) {
        if (test_lgl[j] == TRUE) {

          slice_indices.push_back(old_indices[j] + 1);
          k++;

        }
      }
      if (old_k == k) {
        rows[i] = Rf_allocVector(INTSXP, 0);
      } else {
        rows[i] = Rcpp::IntegerVectorView(Rcpp::seq(old_k + 1, k));
      }

      ++git;
    }
  }

  // the total number of rows
  // only makes sense when the object is fully trained
  inline int size() const {
    return k;
  }

  // once this has been trained on all groups
  // this materialize indices and rows
  void process() {
    indices = Rcpp::wrap(slice_indices);
  }

};

template <typename SlicedTibble>
Rcpp::DataFrame slice_template(const SlicedTibble& gdf, const dplyr::Quosure& quo) {
  typedef typename SlicedTibble::group_iterator group_iterator;
  typedef typename SlicedTibble::slicing_index slicing_index ;

  DataMask<SlicedTibble> mask(gdf);

  const Rcpp::DataFrame& data = gdf.data() ;
  int ngroups = gdf.ngroups() ;
  SymbolVector names(Rf_getAttrib(data, symbols::names));

  GroupSliceIndices<SlicedTibble> group_indices(gdf);

  group_iterator git = gdf.group_begin();
  mask.setup();

  for (int i = 0; i < ngroups; i++, ++git) {
    const slicing_index& indices = *git;

    // empty group size. no need to evaluate the expression
    if (indices.size() == 0) {
      group_indices.empty_group(i) ;
      continue;
    }

    // evaluate the expression in the data mask
    Rcpp::Shield<SEXP> res(mask.eval(quo, indices));
    check_slice_result(res);
    Rcpp::IntegerVector g_positions(res);

    // scan the results to see if all >= 1 or all <= -1
    CountIndices counter(indices.size(), g_positions);

    if (counter.is_positive()) {
      group_indices.add_group_slice_positive(i, g_positions);
    } else if (counter.is_negative()) {
      group_indices.add_group_slice_negative(i, g_positions);
    } else {
      group_indices.empty_group(i);
    }
  }
  group_indices.process();

  Rcpp::Shield<SEXP> quo_env(quo.env());
  return structure_filter(gdf, group_indices, quo_env);
}

}

// [[Rcpp::export(rng = false)]]
SEXP slice_impl(Rcpp::DataFrame df, dplyr::Quosure quosure) {
  if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::slice_template<dplyr::GroupedDataFrame>(dplyr::GroupedDataFrame(df), quosure);
  } else {
    return dplyr::slice_template<dplyr::NaturalDataFrame>(dplyr::NaturalDataFrame(df), quosure);
  }
}
