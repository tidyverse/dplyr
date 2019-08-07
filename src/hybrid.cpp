#include "pch.h"

#include <tools/hash.h>

#include <dplyr/main.h>

#include <dplyr/hybrid/Expression.h>

namespace base {
static SEXP primitive_bracket_one;
static SEXP primitive_bracket_two;

SEXP bracket_one() {
  return primitive_bracket_one;
}
SEXP bracket_two() {
  return primitive_bracket_two;
}
}

namespace dplyr {
namespace hybrid {

// key = actual function
static dplyr_hash_map<SEXP, hybrid_function> hybrid_inline_map;

// key = function name, need this for the pkg::fun case
static dplyr_hash_map<SEXP, hybrid_function> hybrid_named_map;

inline SEXP force(SEXP x) {
  if (TYPEOF(x) == PROMSXP) {
    x = Rf_eval(x, R_BaseEnv);
  }
  return x;
}

dplyr_hash_map<SEXP, hybrid_function>& get_hybrid_inline_map() {
  return hybrid_inline_map;
}
dplyr_hash_map<SEXP, hybrid_function>& get_hybrid_named_map() {
  return hybrid_named_map;
}

void hybrid_init(SEXP env, SEXP name, SEXP package, hybrid_id id) {
  Rcpp::Shield<SEXP> fun(Rf_findVarInFrame3(env, name, FALSE));
  hybrid_inline_map.insert(
    std::make_pair(
      force(fun),
      hybrid_function(name, package, id)
    )
  );
  hybrid_named_map.insert(
    std::make_pair(
      name,
      hybrid_function(name, package, id)
    )
  );
}

void init() {
  if (hybrid_inline_map.size() == 0) {
    Rcpp::Environment dplyr = Rcpp::Environment::namespace_env("dplyr");
    hybrid_init(dplyr, symbols::n, symbols::dplyr, hybrid::N);
    hybrid_init(dplyr, symbols::group_indices, symbols::dplyr, hybrid::GROUP_INDICES);
    hybrid_init(dplyr, symbols::row_number, symbols::dplyr, hybrid::ROW_NUMBER);
    hybrid_init(dplyr, symbols::first, symbols::dplyr, hybrid::FIRST);
    hybrid_init(dplyr, symbols::last, symbols::dplyr, hybrid::LAST);
    hybrid_init(dplyr, symbols::nth, symbols::dplyr, hybrid::NTH);
    hybrid_init(dplyr, symbols::ntile, symbols::dplyr, hybrid::NTILE);
    hybrid_init(dplyr, symbols::min_rank, symbols::dplyr, hybrid::MIN_RANK);
    hybrid_init(dplyr, symbols::percent_rank, symbols::dplyr, hybrid::PERCENT_RANK);
    hybrid_init(dplyr, symbols::dense_rank, symbols::dplyr, hybrid::DENSE_RANK);
    hybrid_init(dplyr, symbols::cume_dist, symbols::dplyr, hybrid::CUME_DIST);
    hybrid_init(dplyr, symbols::lead, symbols::dplyr, hybrid::LEAD);
    hybrid_init(dplyr, symbols::lag, symbols::dplyr, hybrid::LAG);

    SEXP base = R_BaseEnv;
    hybrid_init(base, symbols::sum, symbols::base, hybrid::SUM);
    hybrid_init(base, symbols::mean, symbols::base, hybrid::MEAN);
    hybrid_init(base, symbols::min, symbols::base, hybrid::MIN);
    hybrid_init(base, symbols::max, symbols::base, hybrid::MAX);
    hybrid_init(base, symbols::in, symbols::base, hybrid::IN);

    Rcpp::Environment stats = Rcpp::Environment::namespace_env("stats");
    hybrid_init(stats, symbols::var, symbols::stats, hybrid::VAR);
    hybrid_init(stats, symbols::sd, symbols::stats, hybrid::SD);
  }

  ::base::primitive_bracket_one = Rf_eval(R_BracketSymbol, R_BaseEnv);
  ::base::primitive_bracket_two = Rf_eval(R_Bracket2Symbol, R_BaseEnv);
}

}
}

// [[Rcpp::init]]
void init_hybrid_inline_map(DllInfo* /*dll*/) {
  dplyr::hybrid::init();
}

// [[Rcpp::export(rng = false)]]
Rcpp::List hybrids() {
  int n = dplyr::hybrid::hybrid_inline_map.size();

  Rcpp::CharacterVector names(n);
  Rcpp::CharacterVector packages(n);
  Rcpp::List funs(n);

  dplyr_hash_map<SEXP, dplyr::hybrid::hybrid_function>::iterator it = dplyr::hybrid::hybrid_inline_map.begin();
  for (int i = 0; i < n; ++it, ++i) {
    names[i] = PRINTNAME(it->second.name);
    packages[i] = PRINTNAME(it->second.package);
    funs[i] = it->first;
  }

  Rcpp::List out = Rcpp::List::create(
                     Rcpp::_["name"] = names,
                     Rcpp::_["package"] = packages,
                     Rcpp::_["fun"] = funs
                   );
  Rf_classgets(out, dplyr::NaturalDataFrame::classes());
  dplyr::set_rownames(out, n);
  return out;
}
