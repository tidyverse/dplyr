#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/Quosure.h>

#include <dplyr/checks.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

#include <dplyr/subset/LazyGroupedSubsets.h>

#include <dplyr/standard/Gatherer.h>
#include <dplyr/DataMask.h>
#include <dplyr/NamedListAccumulator.h>

#include <tools/bad.h>
#include <tools/set_rownames.h>

#include <dplyr/hybrid/hybrid.h>

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
namespace internal{

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
class MutateCallProxy {
public:
  typedef LazySplitSubsets<Data> Subsets;
  typedef typename Data::slicing_index Index ;

  MutateCallProxy(const Data& data_, Subsets& subsets_, SEXP expr_, SEXP env_, const SymbolString& name_) :
    data(data_),
    subsets(subsets_),
    expr(expr_),
    env(env_),
    name(name_),
    data_mask(subsets, env)
  {}

  SEXP get() {

    // literal NULL
    if (Rf_isNull(expr)) {
      return expr ;
    }

    // a symbol that is in the data, just return it
    if (TYPEOF(expr) == SYMSXP && subsets.has_variable(CHAR(PRINTNAME(expr)))) {
      return subsets.get_variable(CHAR(PRINTNAME(expr)));
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

  DataMask<Data> data_mask ;

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
      return ListGatherer<Data, Subsets, MutateCallProxy> (List(first), indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    } else {
      return Gatherer<Data, Subsets, MutateCallProxy> (first, indices, const_cast<MutateCallProxy&>(*this), data, i, name).collect();
    }

  }


public:

  SEXP get(const Index& indices) {
    return data_mask.eval(expr, indices) ;
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
      variable = MutateCallProxy<Data>(gdf, subsets, quosure.expr(), quosure.env(), name).get() ;
    }

    if (Rf_isNull(variable)) {
      accumulator.rm(name);
      continue;
    }

    if (!Rcpp::traits::same_type<Data, NaturalDataFrame>::value) {
      Rf_setAttrib(variable, R_NamesSymbol, R_NilValue);
    }

    subsets.input(name, variable);
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
