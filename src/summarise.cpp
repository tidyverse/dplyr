#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>
#include <tools/set_rownames.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/standard/GroupedCallReducer.h>

#include <dplyr/NamedListAccumulator.h>
#include <dplyr/Groups.h>

#include <dplyr/hybrid/hybrid.h>

using namespace Rcpp;
using namespace dplyr;

static
SEXP validate_unquoted_value(SEXP value, int nrows, const SymbolString& name) {
  int n = Rf_length(value);
  check_length(n, nrows, "the number of groups", name);

  // Recycle length 1 vectors
  if (n == 1) {
    value = constant_recycle(value, nrows, name);
  }

  return value;
}

SEXP reconstruct_groups(const DataFrame& old_groups, const List& new_indices, const IntegerVector& firsts, SEXP frame) {
  int nv = old_groups.size() - 1 ;
  List out(nv);
  CharacterVector names(nv);
  CharacterVector old_names(old_groups.names());
  for (int i = 0; i < nv - 1; i++) {
    out[i] = column_subset(old_groups[i], firsts, frame);
    names[i] = old_names[i];
  }
  out[nv - 1] = new_indices;
  names[nv - 1] = ".rows";

  set_rownames(out, new_indices.size());
  set_class(out, NaturalDataFrame::classes());
  copy_attrib(out, old_groups, symbols::dot_drop);
  out.attr("names") = names;
  return out ;
}

template <typename SlicedTibble>
void structure_summarise(List& out, const SlicedTibble& df, SEXP frame) {
  set_class(out, NaturalDataFrame::classes());
}

template <>
void structure_summarise<GroupedDataFrame>(List& out, const GroupedDataFrame& gdf, SEXP frame) {
  const DataFrame& df = gdf.data();

  if (gdf.nvars() > 1) {
    copy_class(out, df);
    SymbolVector vars = gdf.get_vars();
    vars.remove(gdf.nvars() - 1);

    DataFrame old_groups = gdf.group_data();
    int nv = gdf.nvars() - 1;
    DataFrameVisitors visitors(old_groups, nv) ;
    int old_nrows = old_groups.nrow();

    // the number of new groups
    int ngroups = 0;

    // sizes of each new group, there are at most old_nrows groups
    std::vector<int> sizes(old_nrows);

    for (int i = 0; i < old_nrows;) {
      // go through one old group
      int start = i++;
      while (i < old_nrows && visitors.equal(start, i)) i++ ;

      sizes[ngroups++] = i - start;
    }

    // collect the new indices, now that we know the size
    List new_indices(ngroups);

    // the first index of each group
    IntegerVector firsts(no_init(ngroups));

    int start = 0;
    for (int i = 0; i < ngroups; i++) {
      firsts[i] = start + 1;

      int n = sizes[i];
      if (n) {
        new_indices[i] = IntegerVectorView(seq(start + 1, start + n));
      } else {
        new_indices[i] = IntegerVectorView(0);
      }

      start += sizes[i];
    }

    // groups
    DataFrame groups = reconstruct_groups(old_groups, new_indices, firsts, frame);
    GroupedDataFrame::set_groups(out, groups);
  } else {
    // clear groups and reset to non grouped classes
    GroupedDataFrame::strip_groups(out);
    out.attr("class") = NaturalDataFrame::classes();
  }
}

template <typename SlicedTibble>
DataFrame summarise_grouped(const DataFrame& df, const QuosureList& dots, SEXP frame) {
  SlicedTibble gdf(df);

  int nexpr = dots.size();
  int nvars = gdf.nvars();
  check_not_groups(dots, gdf);

  LOG_VERBOSE << "copying " << nvars << " variables to accumulator";

  NamedListAccumulator<SlicedTibble> accumulator;
  int i = 0;
  List results(nvars + nexpr);
  for (; i < nvars; i++) {
    LOG_VERBOSE << "copying " << gdf.symbol(i).get_utf8_cstring();
    results[i] = gdf.label(i);
    accumulator.set(gdf.symbol(i), results[i]);
  }

  LOG_VERBOSE <<  "processing " << nexpr << " variables";

  DataMask<SlicedTibble> mask(gdf);
  for (int k = 0; k < nexpr; k++, i++) {
    LOG_VERBOSE << "processing variable " << k;
    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[k];

    LOG_VERBOSE << "processing variable " << quosure.name().get_utf8_cstring();

    RObject result;

    // Unquoted vectors are directly used as column. Expressions are
    // evaluated in each group.
    if (is_vector(quosure.expr())) {
      result = validate_unquoted_value(quosure.expr(), gdf.ngroups(), quosure.name());
    } else {
      result = hybrid::summarise(quosure, gdf, mask);

      // If we could not find a direct Result,
      // we can use a GroupedCallReducer which will callback to R.
      if (result == R_UnboundValue) {
        mask.setup();
        result = GroupedCallReducer<SlicedTibble>(quosure, mask).process(gdf);
      }
    }
    check_not_null(result, quosure.name());
    check_length(Rf_length(result), gdf.ngroups(), "a summary value", quosure.name());

    results[i] = result;
    accumulator.set(quosure.name(), result);
    mask.input_summarised(quosure.name(), result);
  }

  List out = accumulator;
  // so that the attributes of the original tibble are preserved
  // as requested in issue #1064
  copy_most_attributes(out, df);
  out.names() = accumulator.names();

  int nr = gdf.ngroups();
  set_rownames(out, nr);
  structure_summarise<SlicedTibble>(out, gdf, frame) ;
  return out;
}

// [[Rcpp::export]]
SEXP summarise_impl(DataFrame df, QuosureList dots, SEXP frame) {
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return summarise_grouped<RowwiseDataFrame>(df, dots, frame);
  } else if (is<GroupedDataFrame>(df)) {
    return summarise_grouped<GroupedDataFrame>(df, dots, frame);
  } else {
    return summarise_grouped<NaturalDataFrame>(df, dots, frame);
  }
}

template <typename SlicedTibble>
SEXP hybrid_template(DataFrame df, const Quosure& quosure) {
  SlicedTibble gdf(df);

  const Environment& env = quosure.env();
  SEXP expr = quosure.expr();
  DataMask<SlicedTibble> mask(gdf);
  return hybrid::match(expr, gdf, mask, env);
}


// [[Rcpp::export]]
SEXP hybrid_impl(DataFrame df, Quosure quosure) {
  check_valid_colnames(df);

  if (is<RowwiseDataFrame>(df)) {
    return hybrid_template<RowwiseDataFrame >(df, quosure);
  } else if (is<GroupedDataFrame>(df)) {
    return hybrid_template<GroupedDataFrame >(df, quosure);
  } else {
    return hybrid_template<NaturalDataFrame >(df, quosure);
  }
}
