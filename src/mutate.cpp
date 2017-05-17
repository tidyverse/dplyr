#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/Quosure.h>

#include <dplyr/checks.h>

#include <dplyr/GroupedDataFrame.h>

#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/Gatherer.h>
#include <dplyr/NamedListAccumulator.h>

#include <dplyr/bad.h>

using namespace Rcpp;
using namespace dplyr;

template <typename Data>
SEXP structure_mutate(const NamedListAccumulator<Data>& accumulator,
                      const DataFrame& df,
                      CharacterVector classes,
                      bool grouped = true) {
  List res = accumulator;
  set_class(res, classes);
  set_rownames(res, df.nrows());

  if (grouped) {
    copy_vars(res, df);
    res.attr("labels")  = df.attr("labels");
    res.attr("index")  = df.attr("index");
    res.attr("indices") = df.attr("indices");
    res.attr("drop") = df.attr("drop");
    res.attr("group_sizes") = df.attr("group_sizes");
    res.attr("biggest_group_size") = df.attr("biggest_group_size");
  }

  return res;
}

void check_not_groups(const QuosureList&, const RowwiseDataFrame&) {}

void check_not_groups(const QuosureList& quosures, const GroupedDataFrame& gdf) {
  int n = quosures.size();
  for (int i = 0; i < n; i++) {
    if (gdf.has_group(quosures[i].name()))
      bad_col(quosures[i].name(), "can't be modified because it's a grouping variable");
  }
}

static
SEXP validate_unquoted_value(SEXP value, int nrows, SymbolString& name) {
  if (is_vector(value))
    check_length(Rf_length(value), nrows, "the number of rows", name);
  else
    bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(value)));
  return value;
}

DataFrame mutate_not_grouped(DataFrame df, const QuosureList& dots) {
  const int nexpr = dots.size();
  const int nrows = df.nrows();

  NamedListAccumulator<DataFrame> accumulator;
  const int nvars = df.size();
  if (nvars) {
    CharacterVector df_names = df.names();
    for (int i = 0; i < nvars; i++) {
      accumulator.set(df_names[i], df[i]);
    }
  }

  CallProxy call_proxy(df);

  for (int i = 0; i < nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[i];

    Shield<SEXP> call_(quosure.expr());
    SEXP call = call_;
    SymbolString name = quosure.name();
    Environment env = quosure.env();
    call_proxy.set_env(env);

    RObject variable;
    if (TYPEOF(call) == SYMSXP) {
      SymbolString call_name = SymbolString(Symbol(call));
      if (call_proxy.has_variable(call_name)) {
        variable = call_proxy.get_variable(call_name);
      } else {
        variable = shared_SEXP(env.find(call_name.get_string()));
      }
    } else if (TYPEOF(call) == LANGSXP) {
      call_proxy.set_call(call);
      variable = call_proxy.eval();
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, nrows, name));
      variable = gather->collect();
    } else if (Rf_isNull(call)) {
      accumulator.rm(name);
      continue;
    } else {
      variable = validate_unquoted_value(call, nrows, name);
    }

    if (Rf_inherits(variable, "POSIXlt")) {
      bad_col(quosure.name(), "is of unsupported class POSIXlt");
    }

    const int n_res = Rf_length(variable);
    check_supported_type(variable, name);
    check_length(n_res, nrows, "the number of rows", name);

    if (n_res == 1 && nrows != 1) {
      // recycle
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(variable, nrows, name));
      variable = gather->collect();
    }

    call_proxy.input(name, variable);
    accumulator.set(name, variable);
  }
  List res = structure_mutate(accumulator, df, classes_not_grouped(), false);

  return res;
}

template <typename Data, typename Subsets>
DataFrame mutate_grouped(const DataFrame& df, const QuosureList& dots) {
  LOG_VERBOSE << "checking zero rows";

  // special 0 rows case
  if (df.nrows() == 0) {
    DataFrame res = mutate_not_grouped(df, dots);
    copy_vars(res, df);
    set_class(res, get_class(df));
    return Data(res).data();
  }

  LOG_VERBOSE << "initializing proxy";

  typedef GroupedCallProxy<Data, Subsets> Proxy;
  Data gdf(df);
  int nexpr = dots.size();
  check_not_groups(dots, gdf);

  Proxy proxy(gdf);

  LOG_VERBOSE << "copying data to accumulator";

  NamedListAccumulator<Data> accumulator;
  int ncolumns = df.size();
  CharacterVector column_names = df.names();
  for (int i = 0; i < ncolumns; i++) {
    accumulator.set(column_names[i], df[i]);
  }

  LOG_VERBOSE << "processing " << nexpr << " variables";

  for (int i = 0; i < nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[i];

    Environment env = quosure.env();
    Shield<SEXP> call_(quosure.expr());
    SEXP call = call_;
    SymbolString name = quosure.name();
    proxy.set_env(env);

    LOG_VERBOSE << "processing " << name.get_utf8_cstring();

    RObject variable;

    if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
      proxy.set_call(call);
      boost::scoped_ptr<Gatherer> gather(gatherer<Data, Subsets>(proxy, gdf, name));
      variable = gather->collect();
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, gdf.nrows(), name));
      variable = gather->collect();
    } else if (Rf_isNull(call)) {
      accumulator.rm(name);
      continue;
    } else {
      variable = validate_unquoted_value(call, gdf.nrows(), name);
    }

    Rf_setAttrib(variable, R_NamesSymbol, R_NilValue);
    proxy.input(name, variable);
    accumulator.set(name, variable);
  }

  return structure_mutate(accumulator, df, get_class(df));
}


// [[Rcpp::export]]
SEXP mutate_impl(DataFrame df, QuosureList dots) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_not_grouped(df, dots);
  }
}
