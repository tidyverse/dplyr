#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>
#include <tools/set_rownames.h>

#include <dplyr/NamedListAccumulator.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/hybrid/hybrid.h>
#include <dplyr/standard/GroupedCallReducer.h>

namespace dplyr {

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

SEXP reconstruct_groups(const Rcpp::DataFrame& old_groups, const Rcpp::List& new_indices, const Rcpp::IntegerVector& firsts, SEXP frame) {
  int nv = old_groups.size() - 1 ;
  Rcpp::Shield<SEXP> out(Rf_allocVector(VECSXP, nv));
  Rcpp::Shield<SEXP> names(Rf_allocVector(STRSXP, nv));
  Rcpp::Shield<SEXP> old_names(Rf_getAttrib(old_groups, symbols::names));
  for (int i = 0; i < nv - 1; i++) {
    SET_VECTOR_ELT(out, i, column_subset(old_groups[i], firsts, frame));
    SET_STRING_ELT(names, i, STRING_ELT(old_names, i));
  }
  SET_VECTOR_ELT(out, nv - 1, new_indices);
  SET_STRING_ELT(names, nv - 1, Rf_mkChar(".rows"));

  set_rownames(out, new_indices.size());
  set_class(out, NaturalDataFrame::classes());
  copy_attrib(out, old_groups, symbols::dot_drop);
  Rf_namesgets(out, names);
  return out ;
}

template <typename SlicedTibble>
void structure_summarise(Rcpp::List& out, const SlicedTibble& df, SEXP frame) {
  set_class(out, NaturalDataFrame::classes());
}

template <>
void structure_summarise<GroupedDataFrame>(Rcpp::List& out, const GroupedDataFrame& gdf, SEXP frame) {
  const Rcpp::DataFrame& df = gdf.data();

  if (gdf.nvars() > 1) {
    copy_class(out, df);
    SymbolVector vars = gdf.get_vars();
    vars.remove(gdf.nvars() - 1);

    Rcpp::DataFrame old_groups = gdf.group_data();
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
    Rcpp::List new_indices(ngroups);

    // the first index of each group
    Rcpp::IntegerVector firsts(Rcpp::no_init(ngroups));

    int start = 0;
    for (int i = 0; i < ngroups; i++) {
      firsts[i] = start + 1;

      int n = sizes[i];
      if (n) {
        new_indices[i] = Rcpp::IntegerVectorView(Rcpp::seq(start + 1, start + n));
      } else {
        new_indices[i] = Rcpp::IntegerVectorView(0);
      }

      start += sizes[i];
    }

    // groups
    Rcpp::DataFrame groups = reconstruct_groups(old_groups, new_indices, firsts, frame);
    GroupedDataFrame::set_groups(out, groups);
  } else {
    // clear groups and reset to non grouped classes
    GroupedDataFrame::strip_groups(out);
    Rf_classgets(out, NaturalDataFrame::classes());
  }
}

template <typename SlicedTibble>
Rcpp::DataFrame summarise_grouped(const Rcpp::DataFrame& df, const QuosureList& dots, SEXP frame, SEXP caller_env) {
  SlicedTibble gdf(df);

  int nexpr = dots.size();
  int nvars = gdf.nvars();
  gdf.check_not_groups(dots);

  LOG_VERBOSE << "copying " << nvars << " variables to accumulator";

  NamedListAccumulator<SlicedTibble> accumulator;
  int i = 0;
  Rcpp::List results(nvars + nexpr);
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

    Rcpp::RObject result;

    // Unquoted vectors are directly used as column. Expressions are
    // evaluated in each group.
    Rcpp::Shield<SEXP> quo_expr(quosure.expr());
    if (is_vector(quo_expr)) {
      result = validate_unquoted_value(quo_expr, gdf.ngroups(), quosure.name());
    } else {
      result = hybrid::summarise(quosure, gdf, mask, caller_env);

      // If we could not find a direct Result,
      // we can use a GroupedCallReducer which will callback to R.
      if (result == dplyr::vectors::unbound_sentinel) {
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

  Rcpp::List out = accumulator;
  // so that the attributes of the original tibble are preserved
  // as requested in issue #1064
  copy_most_attributes(out, df);
  Rf_namesgets(out, accumulator.names().get_vector());

  int nr = gdf.ngroups();
  set_rownames(out, nr);
  structure_summarise<SlicedTibble>(out, gdf, frame) ;
  return out;
}

}

// [[Rcpp::export(rng = false)]]
SEXP summarise_impl(Rcpp::DataFrame df, dplyr::QuosureList dots, SEXP frame, SEXP caller_env) {
  check_valid_colnames(df);
  if (Rcpp::is<dplyr::RowwiseDataFrame>(df)) {
    return dplyr::summarise_grouped<dplyr::RowwiseDataFrame>(df, dots, frame, caller_env);
  } else if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::summarise_grouped<dplyr::GroupedDataFrame>(df, dots, frame, caller_env);
  } else {
    return dplyr::summarise_grouped<dplyr::NaturalDataFrame>(df, dots, frame, caller_env);
  }
}

namespace dplyr {

template <typename SlicedTibble>
SEXP hybrid_template(Rcpp::DataFrame df, const Quosure& quosure, SEXP caller_env) {
  SlicedTibble gdf(df);

  Rcpp::Shield<SEXP> env(quosure.env());
  Rcpp::Shield<SEXP> expr(quosure.expr());
  DataMask<SlicedTibble> mask(gdf);
  return hybrid::match(expr, gdf, mask, env, caller_env);
}

}

// [[Rcpp::export(rng = false)]]
SEXP hybrid_impl(Rcpp::DataFrame df, dplyr::Quosure quosure, SEXP caller_env) {
  check_valid_colnames(df);

  if (Rcpp::is<dplyr::RowwiseDataFrame>(df)) {
    return dplyr::hybrid_template<dplyr::RowwiseDataFrame >(df, quosure, caller_env);
  } else if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::hybrid_template<dplyr::GroupedDataFrame >(df, quosure, caller_env);
  } else {
    return dplyr::hybrid_template<dplyr::NaturalDataFrame >(df, quosure, caller_env);
  }
}
