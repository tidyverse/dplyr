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
  DataMask(const Data& data_, Subsets& subsets_, const Environment& env_):
    data(data_),
    subsets(subsets_),
    env(env_),

    mask_promises(child_env(env)),
    mask_bottom(child_env(mask_promises)),

    promises(subsets.size()),
    names(subsets.get_variable_names().get_vector()),
    hybrids(3)
  {
    mask_bottom[".data"] = internal::rlang_api().as_data_pronoun(mask_promises);
    overscope = internal::rlang_api().new_data_mask(mask_bottom, mask_promises, env);

    install_hybrid_functions();
  }

  SEXP eval(SEXP expr, const Index& indices) {
    subsets.clear();
    update_hybrid_functions(indices);

    if (indices.group() == 0) {
      set_promises(indices);
    } else {
      update_promises(indices);
    }

    // modify hybrid functions, n(), ...

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


  SEXP child_env(SEXP parent){
    static SEXP symb_new_env = Rf_install("new.env");
    return Rf_eval(Rf_lang3( symb_new_env, Rf_ScalarLogical(TRUE), parent), R_BaseEnv );
  }

  void set_promises(const Index& indices) {
    static SEXP symbol_column = Rf_install("column");
    for (int i=0; i<names.size(); i++){
      install_promise(i, indices);
    }
  }

  void update_promises(const Index& indices) {
    for (int i=0; i<names.size(); i++){
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
  void delayedAssign(SEXP name, SEXP variable, SEXP indices){
    static SEXP symb_delayedAssign = Rf_install("delayedAssign");

    SEXP col_name = PROTECT(Rf_ScalarString(name));
    SEXP subset_expr = PROTECT(Rf_lang3(R_BracketSymbol, variable, indices));
    SEXP delayedAssignCall = PROTECT(Rf_lang5( symb_delayedAssign, col_name, subset_expr, R_BaseEnv, mask_promises));
    PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
    UNPROTECT(4);
  }

  bool promise_was_forced(SEXP p) {
    return PRVALUE(p) != R_UnboundValue;
  }

  void install_promise(int i, const Index& indices) {
    SEXP name = names[i];
    SEXP column = subsets.get_variable(i);
    delayedAssign( name, column, indices);
    promises[i] = Rf_findVarInFrame( mask_promises, Rf_installChar(name));
  }

  void update_promise(SEXP p, const Index& indices) {
    SEXP code = PRCODE(p);
    SETCADDR(code, indices);
  }

  SEXP new_function(SEXP formals, SEXP body, SEXP env) {
    SEXP function_args = PROTECT(Rf_list2(formals, body));
    SEXP function_call = PROTECT(Rf_lcons(Rf_install("function"), function_args));
    SEXP fn = Rf_eval(function_call, R_BaseEnv);

    UNPROTECT(2);
    return fn;
  }

  void install_hybrid_functions() {
    install_hybrid_n();
    install_hybrid_row_number();
    install_hybrid_group_indices();
  }

  //------------ hybrid installers

  // n <- function() <group size>
  void install_hybrid_n(){
    static SEXP symb_n = Rf_install("n");

    SEXP n_fun = hybrids[0] = PROTECT(new_function(R_NilValue, Rf_ScalarInteger(NA_INTEGER), R_EmptyEnv));
    Rf_defineVar(symb_n, n_fun, mask_bottom) ;
    UNPROTECT(1);
  }

  // row_number <- function(x) if(missing(x)) seq_len(<group size>) else rank(x, ties.method = "first", na.last = "keep")
  void install_hybrid_row_number(){
    static SEXP symb_row_number = Rf_install("row_number");
    static SEXP symb_x = Rf_install("x");
    static SEXP symb_missing = Rf_install("missing");

    SEXP formals = PROTECT(pairlist( _["x"] = R_MissingArg));

    SEXP body = PROTECT(
      Rf_lang4(
        Rf_install("if"),
        Rf_lang2( symb_missing, symb_x),
        Language( "seq_len", 3),
        Language( "rank", symb_x, _["ties.method"] = "first", _["na.last"] = "keep" )
      )
    );

    SEXP fun = hybrids[1] = PROTECT(new_function(formals, body, R_BaseEnv));
    UNPROTECT(3);

    Rf_defineVar(symb_row_number, fun, mask_bottom);
  }

  // group_indices <- function(.data, ...) if(missing(.data)) rep(<group index>, <group_size>) else dplyr::group_indices(.data, ...)
  void install_hybrid_group_indices(){
    static SEXP symb_group_indices = Rf_install("group_indices");
    static SEXP symb_dot_data = Rf_install(".data");
    static SEXP symb_missing = Rf_install("missing");

    SEXP formals = PROTECT(pairlist( _[".data"] = R_MissingArg,  _["..."] = R_MissingArg));

    SEXP body = PROTECT(
      Rf_lang4(
        Rf_install("if"),
        Rf_lang2( symb_missing, symb_dot_data),
        Language( "rep", 1, 2),
        Rf_lang3(
          Rf_lang3(R_DoubleColonSymbol, Rf_install("dplyr"), symb_group_indices),
          symb_dot_data,
          R_DotsSymbol
        )
      )
    );

    SEXP fun = hybrids[2] = PROTECT(new_function(formals, body, R_BaseEnv));
    UNPROTECT(3);

    Rf_defineVar(symb_group_indices, fun, mask_bottom);
  }


  // ----------- hybrid updaters
  void update_hybrid_functions(const Index& indices) {
    update_hybrid_n(indices);
    update_hybrid_row_number(indices);
    update_hybrid_group_indices(indices);
  }

  void update_hybrid_n(const Index& indices) {
    INTEGER(BODY(hybrids[0]))[0] = indices.size();
  }

  void update_hybrid_row_number(const Index& indices) {
    INTEGER(CADR(CADDR(BODY(hybrids[1]))))[0] = indices.size();
  }

  void update_hybrid_group_indices(const Index& indices) {
    SEXP body = BODY(hybrids[2]);
    SEXP rep_call = CADDR(body);
    INTEGER(CADR(rep_call))[0] = indices.group() + 1;
    INTEGER(CADDR(rep_call))[0] = indices.size();
  }

};


template <typename Data, typename Subsets>
class MutateCallProxy {
public:
  MutateCallProxy(const Data& data_, Subsets& subsets_, SEXP expr_, SEXP env_, const SymbolString& name_) :
    data(data_),
    subsets(subsets_),
    expr(expr_),
    env(env_),
    name(name_),
    data_mask(data, subsets, env)
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

}

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

    GroupedDataFrame gdf(df) ;
    if (gdf.ngroups() == 0) {
      return mutate_grouped<NaturalDataFrame, LazySubsets>(df, dots);
    }

    return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>(df, dots);
  } else {
    return mutate_grouped<NaturalDataFrame, LazySubsets>(df, dots);
  }
}
