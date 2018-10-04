#include "pch.h"
#include <dplyr/main.h>
#include <tools/hash.h>
#include <dplyr/hybrid/Expression.h>

namespace dplyr {
namespace hybrid {

static dplyr_hash_map<SEXP, scoped_function> hybrid_inline_map;

inline SEXP force(SEXP x) {
  if (TYPEOF(x) == PROMSXP) {
    x = Rf_eval(x, R_BaseEnv);
  }
  return x;
}

dplyr_hash_map<SEXP, scoped_function>& get_hybrid_inline_map() {
  return hybrid_inline_map;
}

void hybrid_inline_map_insert(SEXP env, SEXP name, SEXP package) {
  hybrid_inline_map.insert(
    std::make_pair<SEXP, scoped_function>(
      force(Rf_findVarInFrame3(env, name, FALSE)),
      scoped_function(name, package)
    )
  );
}

}
}

// [[Rcpp::init]]
void init_hybrid_inline_map(DllInfo* dll) {
  using namespace dplyr::hybrid;

  if (hybrid_inline_map.size() == 0) {
    Environment dplyr = Environment::namespace_env("dplyr");

    hybrid_inline_map_insert(dplyr, symbols::n, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::group_indices, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::row_number, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::first, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::last, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::nth, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::ntile, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::min_rank, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::percent_rank, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::dense_rank, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::cume_dist, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::lead, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::lag, symbols::dplyr);
    hybrid_inline_map_insert(dplyr, symbols::n_distinct, symbols::dplyr);

    SEXP base = R_BaseEnv;
    hybrid_inline_map_insert(base, symbols::sum, symbols::base);
    hybrid_inline_map_insert(base, symbols::mean, symbols::base);
    hybrid_inline_map_insert(base, symbols::min, symbols::base);
    hybrid_inline_map_insert(base, symbols::max, symbols::base);
    hybrid_inline_map_insert(base, symbols::in, symbols::base);

    Environment stats = Environment::namespace_env("stats");
    hybrid_inline_map_insert(stats, symbols::var, symbols::stats);
    hybrid_inline_map_insert(stats, symbols::sd, symbols::stats);
  }
}

// [[Rcpp::export]]
List hybrids() {
  using namespace dplyr::hybrid;

  int n = hybrid_inline_map.size();

  CharacterVector names(n);
  CharacterVector packages(n);
  List funs(n);

  dplyr_hash_map<SEXP, scoped_function>::iterator it = hybrid_inline_map.begin();
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
  out.attr("class") = NaturalDataFrame::classes();
  set_rownames(out, n);
  return out;
}
