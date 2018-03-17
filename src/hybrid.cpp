#include "pch.h"
#include <dplyr/main.h>

#include <tools/hash.h>

#include <dplyr/Hybrid.h>
#include <dplyr/HybridHandlerMap.h>

#include <dplyr/Result/ILazySubsets.h>
#include <dplyr/Result/Rank.h>
#include <dplyr/Result/ConstantResult.h>
#include <dplyr/Result/GroupedHybridCall.h>

using namespace Rcpp;
using namespace dplyr;

bool has_no_class(const RObject& arg) {
  return RCPP_GET_CLASS(arg) == R_NilValue;
}

bool hybridable(RObject arg) {
  if (Rf_inherits(arg, "Date") || Rf_inherits(arg, "POSIXct") || Rf_inherits(arg, "difftime")) return true;

  if (arg.isObject() || arg.isS4()) return false;
  int type = arg.sexp_type();
  switch (type) {
  case INTSXP:
  case REALSXP:
  case LGLSXP:
  case STRSXP:
  case CPLXSXP:
  case RAWSXP:
    return has_no_class(arg);
  default:
    break;
  }
  return false;
}

template <template <int> class Templ>
Result* cumfun_prototype(SEXP call, const ILazySubsets& subsets, int nargs) {
  if (nargs != 1) return 0;
  RObject data(CADR(call));
  if (TYPEOF(data) == SYMSXP) {
    data = subsets.get_variable(SymbolString(Symbol(data)));
  }
  switch (TYPEOF(data)) {
  case INTSXP:
    return new Templ<INTSXP>(data);
  case REALSXP:
    return new Templ<REALSXP>(data);
  default:
    break;
  }
  return 0;
}

HybridHandlerMap& get_handlers() {
  static HybridHandlerMap handlers;
  if (!handlers.size()) {
    /*
    handlers[ Rf_install( "cumsum")      ] = cumfun_prototype<CumSum>;
    handlers[ Rf_install( "cummin")      ] = cumfun_prototype<CumMin>;
    handlers[ Rf_install( "cummax")      ] = cumfun_prototype<CumMax>;
    */

    install_simple_handlers(handlers);
    install_minmax_handlers(handlers);
    install_count_handlers(handlers);
    install_nth_handlers(handlers);
    install_window_handlers(handlers);
    install_offset_handlers(handlers);
    install_in_handlers(handlers);
    install_debug_handlers(handlers);
  }
  return handlers;
}

Result* constant_handler(SEXP constant) {
  switch (TYPEOF(constant)) {
  case INTSXP:
  {
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<INTSXP>(constant, get_date_classes());
    return new ConstantResult<INTSXP>(constant);
  }
  case REALSXP:
  {
    if (Rf_inherits(constant, "difftime")) return new DifftimeConstantResult<REALSXP>(constant);
    if (Rf_inherits(constant, "POSIXct")) return new TypedConstantResult<REALSXP>(constant, get_time_classes());
    if (Rf_inherits(constant, "Date")) return new TypedConstantResult<REALSXP>(constant, get_date_classes());
    return new ConstantResult<REALSXP>(constant);
  }
  case STRSXP:
    return new ConstantResult<STRSXP>(constant);
  case LGLSXP:
    return new ConstantResult<LGLSXP>(constant);
  case CPLXSXP:
    return new ConstantResult<CPLXSXP>(constant);
  default:
    return 0;
  }
}

class VariableResult : public Result {
public:
  VariableResult(const ILazySubsets& subsets_, const SymbolString& name_) : subsets(subsets_), name(name_)  {}

  SEXP process(const GroupedDataFrame&) {
    if (subsets.is_summary(name)) {
      // No need to check length since the summary has already been checked
      return subsets.get_variable(name);
    } else {
      stop("VariableResult::process() needs a summary variable");
    }
  }

  SEXP process(const RowwiseDataFrame&) {
    return subsets.get_variable(name);
  }

  virtual SEXP process(const FullDataFrame&) {
    return subsets.get_variable(name);
  }

  virtual SEXP process(const SlicingIndex& index) {
    return subsets.get(name, index);
  }

private:
  const ILazySubsets& subsets;
  SymbolString name;
};

Result* variable_handler(const ILazySubsets& subsets, const SymbolString& variable) {
  return new VariableResult(subsets, variable);
}

void registerHybridHandler(const char* name, HybridHandler proto) {
  get_handlers()[Rf_install(name)] = proto;
}

namespace dplyr {

Result* get_handler(SEXP call, const ILazySubsets& subsets, const Environment& env) {
  LOG_INFO << "Looking up hybrid handler for call of type " << type2name(call);

  if (TYPEOF(call) == LANGSXP) {
    int depth = Rf_length(call);

    HybridHandlerMap& handlers = get_handlers();

    // interpret dplyr::fun() as fun(). #3309
    SEXP fun_symbol = CAR(call);
    if (TYPEOF(fun_symbol) == LANGSXP &&
        CAR(fun_symbol) == R_DoubleColonSymbol &&
        CADR(fun_symbol) == Rf_install("dplyr")
       ) {
      fun_symbol = CADDR(fun_symbol) ;
    }

    if (TYPEOF(fun_symbol) != SYMSXP) {
      LOG_VERBOSE << "Not a function: " << type2name(fun_symbol);
      return 0;
    }

    LOG_VERBOSE << "Searching hybrid handler for function " << CHAR(PRINTNAME(fun_symbol));

    HybridHandlerMap::const_iterator it = handlers.find(fun_symbol);
    if (it == handlers.end()) {
      LOG_VERBOSE << "Not found";
      return 0;
    }

    LOG_INFO << "Using hybrid handler for " << CHAR(PRINTNAME(fun_symbol));

    return it->second(call, subsets, depth - 1);
  } else if (TYPEOF(call) == SYMSXP) {
    SymbolString sym = SymbolString(Symbol(call));

    LOG_VERBOSE << "Searching hybrid handler for symbol " << sym.get_utf8_cstring();

    if (subsets.has_variable(sym)) {
      if (!subsets.is_summary(sym)) return 0;

      LOG_VERBOSE << "Using hybrid variable handler";
      return variable_handler(subsets, sym);
    }
    else {
      SEXP data;
      try {
        data = env.find(sym.get_string());
      } catch (Rcpp::binding_not_found) {
        return NULL;
      }

      // Constants of length != 1 are handled via regular evaluation
      if (Rf_length(data) == 1) {
        LOG_VERBOSE << "Using hybrid constant handler";
        return constant_handler(data);
      }
    }
  } else {
    // TODO: perhaps deal with SYMSXP separately
    if (Rf_length(call) == 1) return constant_handler(call);
  }
  return 0;
}

SEXP dplyr_object(const char* name) {
  static Environment dplyr = Rcpp::Environment::namespace_env("dplyr");
  return dplyr[name];
}

IHybridCallback::~IHybridCallback() {
}

GroupedHybridEnv::GroupedHybridEnv(const CharacterVector& names_, const Environment& env_,
                                   const IHybridCallback* callback_) :
  names(names_), env(env_), callback(callback_), has_overscope(false)
{
  LOG_VERBOSE;
}

GroupedHybridEnv::~GroupedHybridEnv() {
  if (has_overscope) {
    // We need to call into R because there is no C API for removing
    // bindings from environments
    static Function env_wipe = dplyr_object("env_wipe");
    env_wipe(mask_active);
    env_wipe(mask_bottom);
  }
}

const Environment& GroupedHybridEnv::get_overscope() const {
  provide_overscope();
  return overscope;
}

void GroupedHybridEnv::provide_overscope() const {
  if (has_overscope)
    return;

  // Environment::new_child() performs an R callback, creating the environment
  // in R should be slightly faster
  mask_active =
    create_env_string(
      names, &GroupedHybridEnv::hybrid_get_callback,
      PAYLOAD(const_cast<void*>(reinterpret_cast<const void*>(callback))), env);

  // If bindr (via bindrcpp) supported the creation of a child environment, we could save the
  // call to Rcpp_eval() triggered by mask_active.new_child()
  mask_bottom = mask_active.new_child(true);
  mask_bottom[".data"] = internal::rlang_api().as_data_pronoun(mask_active);

  // Install definitions for formula self-evaluation and unguarding
  overscope = internal::rlang_api().new_data_mask(mask_bottom, mask_active, env);

  has_overscope = true;
}

SEXP GroupedHybridEnv::hybrid_get_callback(const String& name, bindrcpp::PAYLOAD payload) {
  LOG_VERBOSE;
  IHybridCallback* callback_ = reinterpret_cast<IHybridCallback*>(payload.p);
  return callback_->get_subset(SymbolString(name));
}

GroupedHybridCall::GroupedHybridCall(const Call& call_, const ILazySubsets& subsets_, const Environment& env_) :
  original_call(call_), subsets(subsets_), env(env_)
{
  LOG_VERBOSE;
}

// FIXME: replace the search & replace logic with overscoping
Call GroupedHybridCall::simplify(const SlicingIndex& indices) const {
  set_indices(indices);
  Call call = clone(original_call);
  while (simplified(call)) {}
  clear_indices();
  return call;
}

bool GroupedHybridCall::simplified(Call& call) const {
  LOG_VERBOSE;
  // initial
  if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
    boost::scoped_ptr<Result> res(get_handler(call, subsets, env));
    if (res) {
      // replace the call by the result of process
      call = res->process(get_indices());

      // no need to go any further, we simplified the top level
      return true;
    }
    if (TYPEOF(call) == LANGSXP)
      return replace(CDR(call));
  }
  return false;
}

bool GroupedHybridCall::replace(SEXP p) const {
  LOG_VERBOSE;
  SEXP obj = CAR(p);
  if (TYPEOF(obj) == LANGSXP) {
    boost::scoped_ptr<Result> res(get_handler(obj, subsets, env));
    if (res) {
      SETCAR(p, res->process(get_indices()));
      return true;
    }

    if (replace(CDR(obj))) return true;
  }

  if (TYPEOF(p) == LISTSXP) {
    return replace(CDR(p));
  }

  return false;
}

const SlicingIndex& GroupedHybridCall::get_indices() const {
  return *indices;
}

void GroupedHybridCall::set_indices(const SlicingIndex& indices_) const {
  indices = &indices_;
}

void GroupedHybridCall::clear_indices() const {
  indices = NULL;
}

GroupedHybridEval::GroupedHybridEval(const Call& call_, const ILazySubsets& subsets_, const Environment& env_) :
  indices(NULL), subsets(subsets_), env(env_),
  hybrid_env(subsets_.get_variable_names().get_vector(), env_, this),
  hybrid_call(call_, subsets_, env_)
{
  LOG_VERBOSE;
}

const SlicingIndex& GroupedHybridEval::get_indices() const {
  return *indices;
}

SEXP GroupedHybridEval::get_subset(const SymbolString& name) const {
  LOG_VERBOSE;
  return subsets.get(name, get_indices());
}

SEXP GroupedHybridEval::eval(const SlicingIndex& indices_) {
  set_indices(indices_);
  SEXP ret = eval_with_indices();
  clear_indices();
  return ret;
}

void GroupedHybridEval::set_indices(const SlicingIndex& indices_) {
  indices = &indices_;
}

void GroupedHybridEval::clear_indices() {
  indices = NULL;
}

SEXP GroupedHybridEval::eval_with_indices() {
  Call call = hybrid_call.simplify(get_indices());
  LOG_INFO << type2name(call);

  if (TYPEOF(call) == LANGSXP || TYPEOF(call) == SYMSXP) {
    LOG_VERBOSE << "performing evaluation in overscope";
    return Rcpp_eval(call, hybrid_env.get_overscope());
  }
  return call;
}

}
