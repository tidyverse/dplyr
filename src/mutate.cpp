#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/LazyDots.h>

#include <dplyr/checks.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/tbl_cpp.h>

#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/Gatherer.h>
#include <dplyr/Replicator.h>
#include <dplyr/NamedListAccumulator.h>

using namespace Rcpp;
using namespace dplyr;

template <typename Data>
SEXP structure_mutate(const NamedListAccumulator<Data>& accumulator, const DataFrame& df, CharacterVector classes) {
  List res = accumulator;
  set_class(res, classes);
  set_rownames(res, df.nrows());
  res.attr("vars")   = df.attr("vars");
  res.attr("labels")  = df.attr("labels");
  res.attr("index")  = df.attr("index");
  res.attr("indices") = df.attr("indices");
  res.attr("drop") = df.attr("drop");
  res.attr("group_sizes") = df.attr("group_sizes");
  res.attr("biggest_group_size") = df.attr("biggest_group_size");

  return res;
}

void check_not_groups(const LazyDots& dots, const FullDataFrame& gdf) {}

void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf) {}

void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf) {
  int n = dots.size();
  for (int i=0; i<n; i++) {
    if (gdf.has_group(dots[i].name()))
      stop("cannot modify grouping variable");
  }
}


template <typename Data, typename Subsets>
SEXP mutate_grouped(const DataFrame& df, const LazyDots& dots) {
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
  for (int i=0; i<ncolumns; i++) {
    accumulator.set(Symbol(column_names[i]), df[i]);
  }

  LOG_VERBOSE << "processing " << nexpr << " variables";

  List variables(nexpr);
  for (int i=0; i<nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const Lazy& lazy = dots[i];

    Environment env = lazy.env();
    Shield<SEXP> call_(lazy.expr());
    SEXP call = call_;
    Symbol name = lazy.name();
    proxy.set_env(env);

    LOG_VERBOSE << "processing " << CharacterVector(name);

    if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
      proxy.set_call(call);
      boost::scoped_ptr<Gatherer> gather(gatherer<Data, Subsets>(proxy, gdf, name));
      SEXP variable = variables[i] = gather->collect();
      proxy.input(name, variable);
      accumulator.set(name, variable);
    } else if (Rf_length(call) == 1) {
      boost::scoped_ptr<Gatherer> gather(constant_gatherer(call, gdf.nrows()));
      SEXP variable = variables[i] = gather->collect();
      proxy.input(name, variable);
      accumulator.set(name, variable);
    } else if (Rf_isNull(call)) {
      accumulator.rm(name);
      continue;
    } else {
      stop("cannot handle");
    }
  }

  return structure_mutate(accumulator, df, get_class(df));
}


// [[Rcpp::export]]
SEXP mutate_impl(DataFrame df, LazyDots dots) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_grouped<FullDataFrame, LazySubsets>(df, dots);
  }
}
