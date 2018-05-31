#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/Quosure.h>

#include <dplyr/checks.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/NaturalDataFrame.h>

#include <dplyr/Result/LazyRowwiseSubsets.h>
#include <dplyr/Result/CallProxy.h>

#include <dplyr/Gatherer.h>
#include <dplyr/NamedListAccumulator.h>

#include <dplyr/bad.h>
#include <dplyr/tbl_cpp.h>

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

template <int RTYPE>
class ConstantRecycler {
public:
  ConstantRecycler(SEXP constant_, int n_) :
    constant(constant_),
    n(n_)
  {}

  inline SEXP collect() {
    Vector<RTYPE> result(n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0] );
    copy_most_attributes(result, constant);
    return result;
  }

private:
  SEXP constant;
  int n ;

};

template <typename Data, typename Subsets>
class MutateCallProxy {
public:
  MutateCallProxy( const Data& data_, const Subsets& subsets_, SEXP expr_, SEXP env_, const SymbolString& name_) :
    data(data_),
    subsets(subsets_),
    expr(expr_),
    env(env_),
    name(name_)
  {}


  SEXP get() const {

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
      return mutate_constant_recycle(expr);
    }

    // something else
    return validate_unquoted_value();
  }

private:
  const Data& data ;

  // where to find subsets of data variables
  const Subsets& subsets ;

  // expression and environment from the quosure
  SEXP expr ;
  SEXP env ;

  const SymbolString& name ;

  typedef typename Data::slicing_index Index ;

  inline SEXP mutate_constant_recycle(SEXP x) const {
    if (Rf_inherits(x, "POSIXlt")) {
      bad_col(name, "is of unsupported class POSIXlt");
    }
    int n = data.nrows();
    switch (TYPEOF(x)) {
    case INTSXP:
      return ConstantRecycler<INTSXP>(x, n).collect();
    case REALSXP:
      return ConstantRecycler<REALSXP>(x, n).collect();
    case LGLSXP:
      return ConstantRecycler<LGLSXP>(x, n).collect();
    case STRSXP:
      return ConstantRecycler<REALSXP>(x, n).collect();
    case CPLXSXP:
      return ConstantRecycler<STRSXP>(x, n).collect();
    case VECSXP:
      return ConstantRecycler<VECSXP>(x, n).collect();
    case RAWSXP:
      return ConstantRecycler<RAWSXP>(x, n).collect();
    default:
      break;
    }
    bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(x)));
  }

  SEXP validate_unquoted_value() const {
    int nrows = data.nrows();
    if (is_vector(expr))
      check_length(Rf_length(expr), nrows, check_length_message<Data>(), name);
    else
      bad_col(name, "is of unsupported type {type}", _["type"] = Rf_type2char(TYPEOF(expr)));
    return expr;
  }


  SEXP evaluate() const {

    const int ng = data.ngroups();
    if (ng == 0) {
      // return constant_gatherer(proxy.get(NaturalSlicingIndex()), 0, name);

      // TODO; need to evaluate the expression with empty index
      return mutate_constant_recycle(Rf_ScalarLogical(NA_LOGICAL));
    }

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

    boost::scoped_ptr<Gatherer> g(gatherer_impl<Data, Subsets, MutateCallProxy>(first, indices, const_cast<MutateCallProxy&>(*this), data, i, name)) ;
    return g->collect();

  }


public:
  SEXP get( const Index& indices ) const {
    // evaluate the call with the indices
    return IntegerVector( indices.size(), indices.group() ) ;
  }

};


template <typename Data, typename Subsets>
DataFrame mutate_grouped(const DataFrame& df, const QuosureList& dots) {
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

  Subsets subsets(gdf) ;

  for (int i = 0; i < nexpr; i++) {
    Rcpp::checkUserInterrupt();
    const NamedQuosure& quosure = dots[i];
    SymbolString name = quosure.name();

    RObject variable = MutateCallProxy<Data, Subsets>(gdf, subsets, quosure.expr(), quosure.env(), name).get() ;

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
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots);
  } else if (is<GroupedDataFrame>(df)) {
    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_grouped<NaturalDataFrame, LazySubsets>(df, dots);
  }
}
