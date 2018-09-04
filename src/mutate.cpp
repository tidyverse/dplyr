#include "pch.h"
#include <dplyr/main.h>

#include <tools/Quosure.h>
#include <tools/bad.h>
#include <tools/set_rownames.h>
#include <tools/all_na.h>

#include <dplyr/checks.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>
#include <dplyr/data/LazySplitSubsets.h>

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

template <typename Data>
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

template <typename Data>
class Gatherer;

template <typename Data>
class ListGatherer;

template <typename Data>
class MutateCallProxy {
public:
  typedef typename Data::slicing_index Index ;

  MutateCallProxy(const Data& data_, LazySplitSubsets<Data>& subsets_, SEXP expr_, const SymbolString& name_) :
    data(data_),
    subsets(subsets_),
    expr(expr_),
    name(name_)
  {}

  SEXP get() {
    // literal NULL
    if (Rf_isNull(expr)) {
      return expr ;
    }

    // a symbol that is in the data, just return it
    if (TYPEOF(expr) == SYMSXP) {
      const ColumnBinding<Data>* subset_data = subsets.maybe_get_subset_binding(CHAR(PRINTNAME(expr)));
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

  const Data& data ;

  // where to find subsets of data variables
  Subsets& subsets ;

  // expression and environment from the quosure
  SEXP expr ;
  SEXP env ;

  const SymbolString& name ;

  SEXP validate_unquoted_value() const {
    int nrows = data.nrows();
    if (is_vector(expr))
      check_length(Rf_length(expr), nrows, check_length_message<Data>(), name);
    else
      bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(expr)));
    return expr;
  }


  SEXP evaluate() {
    const int ng = data.ngroups();

    typename Data::group_iterator git = data.group_begin();
    typename Data::slicing_index indices = *git;

    RObject first(get(indices));

    if (Rf_inherits(first, "POSIXlt")) {
      bad_col(name, "is of unsupported class POSIXlt");
    }

    if (Rf_inherits(first, "data.frame")) {
      bad_col(name, "is of unsupported class data.frame");
    }

    int i = 0;

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
    check_length(Rf_length(first), indices.size(), check_length_message<Data>(), name);

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
      return ListGatherer<Data> (List(first), indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    } else {
      return Gatherer<Data> (first, indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    }

  }


public:

  SEXP get(const Index& indices) {
    return subsets.eval(expr, indices) ;
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


template <typename Data>
class Gatherer {
public:
  typedef typename Data::slicing_index Index;

  Gatherer(const RObject& first, const Index& indices, MutateCallProxy<Data>& proxy_, const Data& gdf_, int first_non_na_, const SymbolString& name_) :
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
    typename Data::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      Shield<SEXP> subset(proxy.get(indices));
      grab(subset, indices);
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
      check_length(n, indices.size(), check_length_message<Data>(), name);
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

  const Data& gdf;
  MutateCallProxy<Data>& proxy;
  Collecter* coll;
  int first_non_na;
  const SymbolString& name;

};

template <typename Data>
class ListGatherer {
public:
  typedef typename Data::slicing_index Index;

  ListGatherer(List first, const Index& indices, MutateCallProxy<Data>& proxy_, const Data& gdf_, int first_non_na_, const SymbolString& name_) :
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
    typename Data::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      List subset(proxy.get(indices));
      grab(subset, indices);
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
      check_length(n, indices.size(), check_length_message<Data>(), name);
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

  const Data& gdf;
  MutateCallProxy<Data>& proxy;
  List data;
  int first_non_na;
  const SymbolString name;

};



}

template <typename Data>
DataFrame mutate_grouped(const DataFrame& df, const QuosureList& dots) {
  LOG_VERBOSE << "initializing proxy";

  Data gdf(df);
  int nexpr = dots.size();
  check_not_groups(dots, gdf);

  LOG_VERBOSE << "copying data to accumulator";

  NamedListAccumulator<Data> accumulator;
  int ncolumns = df.size();
  CharacterVector column_names = df.names();
  for (int i = 0; i < ncolumns; i++) {
    accumulator.set(column_names[i], df[i]);
  }

  LOG_VERBOSE << "processing " << nexpr << " variables";

  LazySplitSubsets<Data> subsets(gdf) ;

  for (int i = 0; i < nexpr; i++) {

    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[i];
    SymbolString name = quosure.name();

    RObject variable = hybrid::window(quosure.expr(), gdf, subsets, quosure.env()) ;

    if (variable == R_UnboundValue) {
      subsets.reset(quosure.env());
      variable = MutateCallProxy<Data>(gdf, subsets, quosure.expr(), name).get() ;
    }

    if (Rf_isNull(variable)) {
      accumulator.rm(name);
      continue;
    }

    if (!Rcpp::traits::same_type<Data, NaturalDataFrame>::value) {
      Rf_setAttrib(variable, R_NamesSymbol, R_NilValue);
    }

    subsets.input_column(name, variable);
    accumulator.set(name, variable);
  }

  // basic structure of the data frame
  List res = accumulator;
  set_class(res, get_class(df));
  set_rownames(res, df.nrows());

  // let the grouping class deal with the rest, e.g. the
  // groups attribute
  return Data(res, gdf).data();
}


// [[Rcpp::export]]
SEXP mutate_impl(DataFrame df, QuosureList dots) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {

    GroupedDataFrame gdf(df);
    if (gdf.ngroups() == 0) {
      DataFrame res = mutate_grouped<NaturalDataFrame>(df, dots);
      res.attr("groups") = df.attr("groups");
      return res;
    }

    return mutate_grouped<GroupedDataFrame>(df, dots);
  } else {
    return mutate_grouped<NaturalDataFrame>(df, dots);
  }
}
