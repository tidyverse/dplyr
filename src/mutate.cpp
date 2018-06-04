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

namespace dplyr {

template <int RTYPE>
class ConstantRecycler {
public:
  ConstantRecycler(SEXP constant_, int n_) :
    constant(constant_),
    n(n_)
  {}

  inline SEXP collect() {
    Vector<RTYPE> result(n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0]);
    copy_most_attributes(result, constant);
    return result;
  }

private:
  SEXP constant;
  int n ;

};

template <typename Data, typename Subsets, typename Index>
class DataMask {
public:
  DataMask(const Data& data_, Subsets& subsets_, const Environment& env_, Environment hybrid_functions_):
    data(data_),
    subsets(subsets_),
    env(env_),

    mask_promises(child_env(env)),
    mask_bottom(child_env(mask_promises)),

    promises(subsets.size()),
    names(subsets.get_variable_names().get_vector()),
    hybrids(4),
    hybrid_functions(hybrid_functions_)
  {
    mask_bottom[".data"] = internal::rlang_api().as_data_pronoun(mask_promises);
    overscope = internal::rlang_api().new_data_mask(mask_bottom, mask_promises, env);

    install_hybrid_functions();
  }

  SEXP eval(SEXP expr, const Index& indices) {
    subsets.clear();

    hybrid_functions["..group_size"] = indices.size();
    hybrid_functions["..group_number"] = indices.group() + 1;

    if (indices.group() == 0) {
      set_promises(indices);
    } else {
      update_promises(indices);
    }

    // evaluate the call in the overscope
    return Rcpp_eval(expr, overscope);
  }

private:
  const Data& data ;
  Subsets& subsets ;
  const Environment& env ;

  List payload ;
  Environment mask_promises;
  Environment mask_bottom;

  Environment overscope;
  std::vector<SEXP> promises;
  CharacterVector names ;
  std::vector<SEXP> hybrids;
  Environment hybrid_functions;


  SEXP child_env(SEXP parent) {
    static SEXP symb_new_env = Rf_install("new.env");
    return Rf_eval(Rf_lang3(symb_new_env, Rf_ScalarLogical(TRUE), parent), R_BaseEnv);
  }

  void set_promises(const Index& indices) {
    for (int i = 0; i < names.size(); i++) {
      install_promise(i, indices);
    }
  }

  void update_promises(const Index& indices) {
    for (int i = 0; i < names.size(); i++) {
      SEXP p = promises[i];
      // in any case it is a promise

      if (promise_was_forced(p)) {
        // it has been evaluated, install a new promise
        // would maybe be better to do either of:
        // - reset it to unforced, but SET_PRVALUE(p, R_UnboundValue) does not work
        // - promote the promise to its value and recalculate it upfront for each group
        install_promise(i, indices);
      } else {
        // otherwise just need to update the expression
        update_promise(p, indices);
      }

    }
  }

  // delayedAssign( {column_name}, x[i], baseenv(), mask_promises )
  void delayedAssign(SEXP name, SEXP variable, SEXP indices) {
    SEXP symb_delayedAssign = Rf_install("delayedAssign");
    SEXP col_name = PROTECT(Rf_ScalarString(name));
    SEXP subset_expr = PROTECT(Rf_lang3(R_BracketSymbol, variable, indices));
    SEXP delayedAssignCall = PROTECT(Rf_lang5(symb_delayedAssign, col_name, subset_expr, R_BaseEnv, mask_promises));
    PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
    UNPROTECT(4);
  }

  bool promise_was_forced(SEXP p) {
    return PRVALUE(p) != R_UnboundValue;
  }

  void install_promise(int i, const Index& indices) {
    SEXP name = names[i];
    SEXP column = subsets.get_variable(i);

    delayedAssign(name, column, indices);
    promises[i] = Rf_findVarInFrame(mask_promises, Rf_installChar(name));
  }

  void update_promise(SEXP p, const Index& indices) {
    SEXP code = PRCODE(p);
    SETCADDR(code, indices);
  }

  void install_hybrid_functions() {
    mask_bottom["n"] = (SEXP)hybrid_functions["n"];
    mask_bottom["row_number"] = (SEXP)hybrid_functions["row_number"];
    mask_bottom["group_indices"] = (SEXP)hybrid_functions["group_indices"];
    mask_bottom["ntile"] = (SEXP)hybrid_functions["ntile"];
  }

};

template <>
void DataMask<RowwiseDataFrame, LazyRowwiseSubsets, RowwiseDataFrame::slicing_index>::delayedAssign(SEXP name, SEXP variable, SEXP indices) {
  SEXP symb_delayedAssign = Rf_install("delayedAssign");
  SEXP col_name = PROTECT(Rf_ScalarString(name));
  SEXP subset_expr = PROTECT(Rf_lang3(R_Bracket2Symbol, variable, indices));  // x[[i]] instead of x[i]
  SEXP delayedAssignCall = PROTECT(Rf_lang5(symb_delayedAssign, col_name, subset_expr, R_BaseEnv, mask_promises));
  PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
  UNPROTECT(4);
}




template <typename Data, typename Subsets>
class MutateCallProxy {
public:
  MutateCallProxy(const Data& data_, Subsets& subsets_, SEXP expr_, SEXP env_, const SymbolString& name_, const Environment& hybrid_functions) :
    data(data_),
    subsets(subsets_),
    expr(expr_),
    env(env_),
    name(name_),
    data_mask(data, subsets, env, hybrid_functions)
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
      return mutate_constant_recycle(expr);
    }

    // something else
    return validate_unquoted_value();
  }

private:
  typedef typename Data::slicing_index Index ;

  const Data& data ;

  // where to find subsets of data variables
  Subsets& subsets ;

  // expression and environment from the quosure
  SEXP expr ;
  SEXP env ;

  const SymbolString& name ;

  DataMask<Data, Subsets, Index> data_mask ;

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

    boost::scoped_ptr<Gatherer> g(gatherer_impl<Data, Subsets, MutateCallProxy>(first, indices, const_cast<MutateCallProxy&>(*this), data, i, name)) ;
    return g->collect();
  }


public:

  SEXP get(const Index& indices) {
    return data_mask.eval(expr, indices) ;
  }

};

template <>
SEXP MutateCallProxy<NaturalDataFrame, LazySubsets>::evaluate() {
  NaturalDataFrame::group_iterator git = data.group_begin();
  NaturalDataFrame::slicing_index indices = *git;

  RObject first(get(indices));

  if (Rf_inherits(first, "POSIXlt")) {
    bad_col(name, "is of unsupported class POSIXlt");
  }

  if (Rf_inherits(first, "data.frame")) {
    bad_col(name, "is of unsupported class data.frame");
  }

  check_supported_type(first, name);
  check_length(Rf_length(first), indices.size(), check_length_message<NaturalDataFrame>(), name);

  if (Rf_length(first) == 1 && indices.size() != 1) {
    return mutate_constant_recycle(first);
  }
  return first;
}

}

template <typename Data, typename Subsets>
DataFrame mutate_grouped(const DataFrame& df, const QuosureList& dots, const Environment& hybrid_functions) {
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

    RObject variable = MutateCallProxy<Data, Subsets>(gdf, subsets, quosure.expr(), quosure.env(), name, hybrid_functions).get() ;

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
SEXP mutate_impl(DataFrame df, QuosureList dots, Environment hybrid_functions) {
  if (dots.size() == 0) return df;
  check_valid_colnames(df);
  if (is<RowwiseDataFrame>(df)) {
    return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>(df, dots, hybrid_functions);
  } else if (is<GroupedDataFrame>(df)) {

    GroupedDataFrame gdf(df);
    if (gdf.ngroups() == 0) {
      DataFrame res = mutate_grouped<NaturalDataFrame, LazySubsets>(df, dots, hybrid_functions);
      res.attr("groups") = df.attr("groups");
      return res;
    }

    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots, hybrid_functions);
  } else {
    return mutate_grouped<NaturalDataFrame, LazySubsets>(df, dots, hybrid_functions);
  }
}
