#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>
#include <tools/bad.h>
#include <tools/set_rownames.h>
#include <tools/all_na.h>

#include <dplyr/checks.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>
#include <dplyr/data/DataMask.h>

#include <dplyr/NamedListAccumulator.h>

#include <dplyr/hybrid/hybrid.h>
#include <dplyr/Collecter.h>

using namespace Rcpp;
using namespace dplyr;

void check_not_groups(const QuosureList&, const RowwiseDataFrame&) {}
void check_not_groups(const QuosureList&, const NaturalDataFrame&) {}

void check_not_groups(const QuosureList& quosures, const GroupedDataFrame& gdf) {
  int n = quosures.size();
  for (int i = 0; i < n; i++) {
    if (gdf.has_group(quosures[i].name()))
      bad_col(quosures[i].name(), "can't be modified because it's a grouping variable");
  }
}

namespace dplyr {

template <typename SlicedTibble>
inline const char* check_length_message() {
  return "the group size";
}
template <>
inline const char* check_length_message<NaturalDataFrame>() {
  return "the number of rows";
}

namespace internal {

template <int RTYPE>
class ConstantRecycler {
public:
  ConstantRecycler(SEXP constant_, int n_) :
    constant(constant_),
    n(n_)
  {}

  inline SEXP collect() {
    Rcpp::Vector<RTYPE> result(n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0]);
    copy_most_attributes(result, constant);
    return result;
  }

private:
  SEXP constant;
  int n ;
};

}

inline SEXP constant_recycle(SEXP x, int n, const SymbolString& name) {
  if (Rf_inherits(x, "POSIXlt")) {
    bad_col(name, "is of unsupported class POSIXlt");
  }
  switch (TYPEOF(x)) {
  case INTSXP:
    return internal::ConstantRecycler<INTSXP>(x, n).collect();
  case REALSXP:
    return internal::ConstantRecycler<REALSXP>(x, n).collect();
  case LGLSXP:
    return internal::ConstantRecycler<LGLSXP>(x, n).collect();
  case STRSXP:
    return internal::ConstantRecycler<STRSXP>(x, n).collect();
  case CPLXSXP:
    return internal::ConstantRecycler<CPLXSXP>(x, n).collect();
  case VECSXP:
    return internal::ConstantRecycler<VECSXP>(x, n).collect();
  case RAWSXP:
    return internal::ConstantRecycler<RAWSXP>(x, n).collect();
  default:
    break;
  }
  bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(x)));
}

template <typename SlicedTibble>
class Gatherer;

template <typename SlicedTibble>
class ListGatherer;

template <typename SlicedTibble>
class MutateCallProxy {
public:
  typedef typename SlicedTibble::slicing_index Index ;

  MutateCallProxy(const SlicedTibble& data_, DataMask<SlicedTibble>& mask_, const NamedQuosure& quosure_) :
    data(data_),
    mask(mask_),
    quosure(quosure_.get()),
    expr(quosure_.expr()),
    name(quosure_.name())
  {}

  SEXP get() {
    // literal NULL
    if (Rf_isNull(expr)) {
      return expr ;
    }

    // a symbol that is in the data, just return it
    if (TYPEOF(expr) == SYMSXP) {
      const ColumnBinding<SlicedTibble>* subset_data = mask.maybe_get_subset_binding(CHAR(PRINTNAME(expr)));
      if (subset_data) return subset_data->get_data();
    }

    // a call or symbol that is not in the data
    if (TYPEOF(expr) == LANGSXP || TYPEOF(expr) == SYMSXP) {
      return evaluate();
    }

    // a constant
    if (Rf_length(expr) == 1) {
      return constant_recycle(expr, data.nrows(), name);
    }

    // something else
    return validate_unquoted_value();
  }

private:

  const SlicedTibble& data ;

  // where to find subsets of data variables
  DataMask<SlicedTibble>& mask ;

  Quosure quosure;

  // expression unwrapped from the quosure
  SEXP expr;

  SymbolString name ;

  SEXP validate_unquoted_value() const {
    int nrows = data.nrows();
    if (is_vector(expr))
      check_length(Rf_length(expr), nrows, check_length_message<SlicedTibble>(), name);
    else
      bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(expr)));
    return expr;
  }


  SEXP evaluate() {
    const int ng = data.ngroups();

    typename SlicedTibble::group_iterator git = data.group_begin();

    int i = 0;
    while (!(*git).size()) {
      ++git;
      i++;
    }

    typename SlicedTibble::slicing_index indices = *git;
    RObject first(get(indices));

    if (Rf_inherits(first, "POSIXlt")) {
      bad_col(name, "is of unsupported class POSIXlt");
    }

    if (Rf_inherits(first, "data.frame")) {
      bad_col(name, "is of unsupported class data.frame");
    }

    if (Rf_isNull(first)) {
      while (Rf_isNull(first)) {
        i++;
        if (i == ng) return R_NilValue;
        ++git;
        indices = *git;
        first = get(indices);
      }
    }
    check_supported_type(first, name);
    check_length(Rf_length(first), indices.size(), check_length_message<SlicedTibble>(), name);

    if (ng > 1) {
      while (all_na(first)) {
        i++;
        if (i == ng) break;
        ++git;
        indices = *git;
        first = get(indices);
      }
    }

    if (TYPEOF(first) == VECSXP) {
      return ListGatherer<SlicedTibble> (List(first), indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    } else {
      return Gatherer<SlicedTibble> (first, indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    }

  }


public:

  SEXP get(const Index& indices) {
    return mask.eval(quosure, indices) ;
  }

};

template <>
SEXP MutateCallProxy<NaturalDataFrame>::evaluate() {
  NaturalDataFrame::group_iterator git = data.group_begin();
  NaturalDataFrame::slicing_index indices = *git;

  RObject first(get(indices));
  if (Rf_isNull(first)) return R_NilValue;

  if (Rf_inherits(first, "POSIXlt")) {
    bad_col(name, "is of unsupported class POSIXlt");
  }

  if (Rf_inherits(first, "data.frame")) {
    bad_col(name, "is of unsupported class data.frame");
  }

  check_supported_type(first, name);
  check_length(Rf_length(first), indices.size(), check_length_message<NaturalDataFrame>(), name);

  if (Rf_length(first) == 1 && indices.size() != 1) {
    return constant_recycle(first, indices.size(), name);
  }
  return first;
}


template <typename SlicedTibble>
class Gatherer {
public:
  typedef typename SlicedTibble::slicing_index Index;

  Gatherer(const RObject& first, const Index& indices, MutateCallProxy<SlicedTibble>& proxy_, const SlicedTibble& gdf_, int first_non_na_, const SymbolString& name_) :
    gdf(gdf_), proxy(proxy_), first_non_na(first_non_na_), name(name_)
  {
    coll = collecter(first, gdf.nrows());
    if (first_non_na < gdf.ngroups())
      grab(first, indices);
  }

  ~Gatherer() {
    if (coll != 0) {
      delete coll;
    }
  }

  SEXP collect() {
    int ngroups = gdf.ngroups();
    if (first_non_na == ngroups) return coll->get();
    typename SlicedTibble::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      if (indices.size()) {
        Shield<SEXP> subset(proxy.get(indices));
        grab(subset, indices);
      }
    }
    return coll->get();
  }

private:

  inline void grab(SEXP subset, const Index& indices) {
    int n = Rf_length(subset);
    if (n == indices.size()) {
      grab_along(subset, indices);
    } else if (n == 1) {
      grab_rep(subset, indices);
    } else if (Rf_isNull(subset)) {
      stop("incompatible types (NULL), expecting %s", coll->describe());
    } else {
      check_length(n, indices.size(), check_length_message<SlicedTibble>(), name);
    }
  }

  template <typename Idx>
  void grab_along(SEXP subset, const Idx& indices) {
    if (coll->compatible(subset)) {
      // if the current source is compatible, collect
      coll->collect(indices, subset);
    } else if (coll->can_promote(subset)) {
      // setup a new Collecter
      Collecter* new_collecter = promote_collecter(subset, gdf.nrows(), coll);

      // import data from previous collecter.
      new_collecter->collect(NaturalSlicingIndex(gdf.nrows()), coll->get());

      // import data from this chunk
      new_collecter->collect(indices, subset);

      // dispose the previous collecter and keep the new one.
      delete coll;
      coll = new_collecter;
    } else if (coll->is_logical_all_na()) {
      Collecter* new_collecter = collecter(subset, gdf.nrows());
      new_collecter->collect(indices, subset);
      delete coll;
      coll = new_collecter;
    } else {
      bad_col(name, "can't be converted from {source_type} to {target_type}",
              _["source_type"] = coll->describe(), _["target_type"] = get_single_class(subset));
    }
  }

  void grab_rep(SEXP value, const Index& indices) {
    int n = indices.size();
    // FIXME: This can be made faster if `source` in `Collecter->collect(source, indices)`
    //        could be of length 1 recycling the value.
    // TODO: create Collecter->collect_one(source, indices)?
    for (int j = 0; j < n; j++) {
      grab_along(value, RowwiseSlicingIndex(indices[j]));
    }
  }

  const SlicedTibble& gdf;
  MutateCallProxy<SlicedTibble>& proxy;
  Collecter* coll;
  int first_non_na;
  const SymbolString& name;

};

template <typename SlicedTibble>
class ListGatherer {
public:
  typedef typename SlicedTibble::slicing_index Index;

  ListGatherer(List first, const Index& indices, MutateCallProxy<SlicedTibble>& proxy_, const SlicedTibble& gdf_, int first_non_na_, const SymbolString& name_) :
    gdf(gdf_), proxy(proxy_), data(gdf.nrows()), first_non_na(first_non_na_), name(name_)
  {
    if (first_non_na < gdf.ngroups()) {
      grab(first, indices);
    }

    copy_most_attributes(data, first);
  }

  SEXP collect() {
    int ngroups = gdf.ngroups();
    if (first_non_na == ngroups) return data;
    typename SlicedTibble::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      if (indices.size()) {
        Shield<SEXP> res(proxy.get(indices));
        List subset(res);
        grab(subset, indices);
      }
    }
    return data;
  }

private:

  inline void grab(const List& subset, const Index& indices) {
    int n = subset.size();

    if (n == indices.size()) {
      grab_along(subset, indices);
    } else if (n == 1) {
      grab_rep(subset[0], indices);
    } else {
      check_length(n, indices.size(), check_length_message<SlicedTibble>(), name);
    }
  }

  void grab_along(const List& subset, const Index& indices) {
    int n = indices.size();
    for (int j = 0; j < n; j++) {
      data[ indices[j] ] = subset[j];
    }
  }

  void grab_rep(SEXP value, const Index& indices) {
    int n = indices.size();
    for (int j = 0; j < n; j++) {
      data[ indices[j] ] = value;
    }
  }

  const SlicedTibble& gdf;
  MutateCallProxy<SlicedTibble>& proxy;
  List data;
  int first_non_na;
  const SymbolString name;

};



}

template <typename SlicedTibble>
DataFrame mutate_grouped(const DataFrame& df, const QuosureList& dots, SEXP caller_env) {
  LOG_DEBUG << "initializing proxy";

  SlicedTibble gdf(df);
  int nexpr = dots.size();
  check_not_groups(dots, gdf);

  LOG_DEBUG << "copying data to accumulator";

  NamedListAccumulator<SlicedTibble> accumulator;
  int ncolumns = df.size();
  Shield<SEXP> column_names(Rf_getAttrib(df, symbols::names));
  for (int i = 0; i < ncolumns; i++) {
    accumulator.set(STRING_ELT(column_names, i), df[i]);
  }

  LOG_VERBOSE << "processing " << nexpr << " variables";

  DataMask<SlicedTibble> mask(gdf) ;

  for (int i = 0; i < nexpr; i++) {

    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[i];
    SymbolString name = quosure.name();

    LOG_VERBOSE << "Variable " << name.get_utf8_cstring();

    RObject variable = hybrid::window(quosure.expr(), gdf, mask, quosure.env(), caller_env) ;

    LOG_VERBOSE << "Checking result";

    if (variable == R_UnboundValue) {
      LOG_VERBOSE << "Rechaining";

      // NULL columns are not removed if `setup()` is not called here
      mask.setup();

      if (quosure.is_rlang_lambda()) {
        // need to create a new quosure to put the data mask in scope
        // of the lambda function
        Shield<SEXP> new_quosure(make_lambda_quosure(quosure, mask.get_data_mask()));
        NamedQuosure lambda_quosure(new_quosure, quosure.name());
        variable = MutateCallProxy<SlicedTibble>(gdf, mask, lambda_quosure).get();
      } else {
        variable = MutateCallProxy<SlicedTibble>(gdf, mask, quosure).get();
      }
    }

    if (Rf_isNull(variable)) {
      accumulator.rm(name);
      mask.rm(name);
      continue;
    }

    LOG_VERBOSE << "Finalizing";

    if (!Rcpp::traits::same_type<SlicedTibble, NaturalDataFrame>::value) {
      Rf_setAttrib(variable, R_NamesSymbol, R_NilValue);
    }

    mask.input_column(name, variable);
    accumulator.set(name, variable);
  }

  // basic structure of the data frame
  List res = accumulator;
  set_class(res, get_class(df));
  set_rownames(res, df.nrows());

  // let the grouping class deal with the rest, e.g. the
  // groups attribute
  return SlicedTibble(res, gdf).data();
}

template <typename SlicedTibble>
SEXP mutate_zero(const DataFrame& df, const QuosureList& dots, SEXP caller_env, bool set_groups) {
  SlicedTibble tbl(df);
  if (tbl.ngroups() == 0 || tbl.nrows() == 0) {
    DataFrame res = mutate_grouped<NaturalDataFrame>(df, dots, caller_env);
    if (set_groups) {
      GroupedDataFrame::copy_groups(res, df);
    }
    return res;
  }
  return mutate_grouped<SlicedTibble>(df, dots, caller_env);
}

// [[Rcpp::export(rng = false)]]
SEXP mutate_impl(DataFrame df, QuosureList dots, SEXP caller_env) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_zero<RowwiseDataFrame>(df, dots, caller_env, false);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_zero<GroupedDataFrame>(df, dots, caller_env, true);
  } else {
    return mutate_grouped<NaturalDataFrame>(df, dots, caller_env);
  }
}
