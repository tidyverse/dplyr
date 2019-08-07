#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>

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

template <typename SlicedTibble>
Rcpp::List summarise_grouped(const Rcpp::DataFrame& df, const QuosureList& dots, SEXP caller_env) {
  SlicedTibble gdf(df);

  int nexpr = dots.size();
  int nvars = gdf.nvars();
  gdf.check_not_groups(dots);

  LOG_VERBOSE << "copying " << nvars << " variables to accumulator";

  NamedListAccumulator<SlicedTibble> accumulator;

  LOG_VERBOSE <<  "processing " << nexpr << " variables";

  DataMask<SlicedTibble> mask(gdf);
  for (int k = 0; k < nexpr; k++) {
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
      if (result == R_UnboundValue) {
        mask.setup();
        result = GroupedCallReducer<SlicedTibble>(quosure, mask).process(gdf);
      }
    }
    check_not_null(result, quosure.name());
    check_length(Rf_length(result), gdf.ngroups(), "a summary value", quosure.name());

    accumulator.set(quosure.name(), result);
    mask.input_summarised(quosure.name(), result);
  }

  return accumulator;
}

}

// [[Rcpp::export(rng = false)]]
SEXP summarise_impl(Rcpp::DataFrame df, dplyr::QuosureList dots, SEXP caller_env) {
  check_valid_colnames(df);
  if (Rcpp::is<dplyr::RowwiseDataFrame>(df)) {
    return dplyr::summarise_grouped<dplyr::RowwiseDataFrame>(df, dots, caller_env);
  } else if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::summarise_grouped<dplyr::GroupedDataFrame>(df, dots, caller_env);
  } else {
    return dplyr::summarise_grouped<dplyr::NaturalDataFrame>(df, dots, caller_env);
  }
}

namespace dplyr {

template <typename SlicedTibble>
SEXP summarise_one_impl(const Rcpp::DataFrame& df, const Rcpp::List& summaries, const dplyr::Quosure& quosure, SEXP caller_env) {
  SlicedTibble gdf(df);

  DataMask<SlicedTibble> mask(gdf);

  // register the summaries
  SEXP summaries_names = Rf_getAttrib(summaries, R_NamesSymbol);
  for (int i = 0; i < summaries.size(); i++) {
    mask.input_summarised(SymbolString(STRING_ELT(summaries_names, i)), summaries[i]);
  }

  mask.setup();

  int ngroups = gdf.ngroups();
  Rcpp::List result(ngroups);
  typename SlicedTibble::group_iterator it = gdf.group_begin();
  for (int i=0; i<ngroups; i++, ++it) {
    result[i] = mask.eval(quosure, *it);
  }

  return result;

}

}




// [[Rcpp::export(rng = false)]]
SEXP summarise_one(Rcpp::DataFrame df, Rcpp::List summaries, dplyr::Quosure quosure, SEXP caller_env) {

  // check_valid_colnames(df);
  if (Rcpp::is<dplyr::RowwiseDataFrame>(df)) {
    return dplyr::summarise_one_impl<dplyr::RowwiseDataFrame>(df, summaries, quosure, caller_env);
  } else if (Rcpp::is<dplyr::GroupedDataFrame>(df)) {
    return dplyr::summarise_one_impl<dplyr::GroupedDataFrame>(df, summaries, quosure, caller_env);
  } else {
    return dplyr::summarise_one_impl<dplyr::NaturalDataFrame>(df, summaries, quosure, caller_env);
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
