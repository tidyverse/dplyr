#include "pch.h"
#include <dplyr/main.h>
#include <tools/hash.h>
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
  Shield<SEXP> fun(Rf_findVarInFrame3(env, name, FALSE));
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

}
}

// [[Rcpp::init]]
void init_hybrid_inline_map(DllInfo* /*dll*/) {
  using namespace dplyr::hybrid;

  if (hybrid_inline_map.size() == 0) {
    Environment dplyr = Environment::namespace_env("dplyr");
    hybrid_init(dplyr, symbols::n, symbols::dplyr, N);
    hybrid_init(dplyr, symbols::group_indices, symbols::dplyr, GROUP_INDICES);
    hybrid_init(dplyr, symbols::row_number, symbols::dplyr, ROW_NUMBER);
    hybrid_init(dplyr, symbols::first, symbols::dplyr, FIRST);
    hybrid_init(dplyr, symbols::last, symbols::dplyr, LAST);
    hybrid_init(dplyr, symbols::nth, symbols::dplyr, NTH);
    hybrid_init(dplyr, symbols::ntile, symbols::dplyr, NTILE);
    hybrid_init(dplyr, symbols::min_rank, symbols::dplyr, MIN_RANK);
    hybrid_init(dplyr, symbols::percent_rank, symbols::dplyr, PERCENT_RANK);
    hybrid_init(dplyr, symbols::dense_rank, symbols::dplyr, DENSE_RANK);
    hybrid_init(dplyr, symbols::cume_dist, symbols::dplyr, CUME_DIST);
    hybrid_init(dplyr, symbols::lead, symbols::dplyr, LEAD);
    hybrid_init(dplyr, symbols::lag, symbols::dplyr, LAG);
    hybrid_init(dplyr, symbols::n_distinct, symbols::dplyr, N_DISTINCT);

    SEXP base = R_BaseEnv;
    hybrid_init(base, symbols::sum, symbols::base, SUM);
    hybrid_init(base, symbols::mean, symbols::base, MEAN);
    hybrid_init(base, symbols::min, symbols::base, MIN);
    hybrid_init(base, symbols::max, symbols::base, MAX);
    hybrid_init(base, symbols::in, symbols::base, IN);

    Environment stats = Environment::namespace_env("stats");
    hybrid_init(stats, symbols::var, symbols::stats, VAR);
    hybrid_init(stats, symbols::sd, symbols::stats, SD);
  }

  ::base::primitive_bracket_one = Rf_eval(R_BracketSymbol, R_BaseEnv);
  ::base::primitive_bracket_two = Rf_eval(R_Bracket2Symbol, R_BaseEnv);
}

// [[Rcpp::export(rng = false)]]
List hybrids() {
  using namespace dplyr::hybrid;

  int n = hybrid_inline_map.size();

  CharacterVector names(n);
  CharacterVector packages(n);
  List funs(n);

  dplyr_hash_map<SEXP, hybrid_function>::iterator it = hybrid_inline_map.begin();
  for (int i = 0; i < n; ++it, ++i) {
    names[i] = PRINTNAME(it->second.name);
    packages[i] = PRINTNAME(it->second.package);
    funs[i] = it->first;
  }

  List out = List::create(
               _["name"] = names,
               _["package"] = packages,
               _["fun"] = funs
             );
  Rf_classgets(out, NaturalDataFrame::classes());
  set_rownames(out, n);
  return out;
}
