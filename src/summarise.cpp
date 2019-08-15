#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/hybrid/hybrid.h>

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
