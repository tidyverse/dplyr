#include <dplyr.h>

#include <tools/match.h>
#include <tools/utils.h>

#include <dplyr/extract_column.h>

#include <dplyr/tbl_cpp.h>
#include <dplyr/Groups.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
SEXP resolve_vars(List new_groups, CharacterVector names) {
  int n = new_groups.size();
  for (int i=0; i<n; i++) {
    List lazy = new_groups[i];
    Environment env = lazy[1];
    SEXP s = lazy[0];

    // expand column
    if (TYPEOF(s) == SYMSXP) {

    } else if (TYPEOF(s) == LANGSXP && CAR(s) == Rf_install("column") && Rf_length(s) == 2) {
      s = extract_column(CADR(s), env);
    } else {
      continue;
    }
    // check that s is indeed in the data

    int pos = as<int>(r_match(CharacterVector::create(PRINTNAME(s)), names));
    if (pos == NA_INTEGER) {
      stop("unknown variable to group by : %s", CHAR(PRINTNAME(s)));
    }
    lazy[0] = s;
  }

  return new_groups;
}

// [[Rcpp::export]]
DataFrame grouped_df_impl(DataFrame data, ListOf<Symbol> symbols, bool drop) {
  assert_all_white_list(data);
  DataFrame copy(shallow_copy(data));
  copy.attr("vars") = symbols;
  copy.attr("drop") = drop;
  if (!symbols.size())
    stop("no variables to group by");
  return build_index_cpp(copy);
}

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  SET_OBJECT(copy, OBJECT(df));
  copy.attr("class") = CharacterVector::create("data.frame");
  return copy;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  SET_ATTRIB(copy, strip_group_attributes(df));
  return copy;
}

SEXP strip_group_attributes(SEXP df) {
  Shield<SEXP> attribs(Rf_cons(dplyr::classes_not_grouped(), R_NilValue));
  SET_TAG(attribs, Rf_install("class"));

  SEXP p = ATTRIB(df);
  std::vector<SEXP> black_list(8);
  black_list[0] = Rf_install("indices");
  black_list[1] = Rf_install("vars");
  black_list[2] = Rf_install("index");
  black_list[3] = Rf_install("labels");
  black_list[4] = Rf_install("drop");
  black_list[5] = Rf_install("group_sizes");
  black_list[6] = Rf_install("biggest_group_size");
  black_list[7] = Rf_install("class");

  SEXP q = attribs;
  while (! Rf_isNull(p)) {
    SEXP tag = TAG(p);
    if (std::find(black_list.begin(), black_list.end(), tag) == black_list.end()) {
      Shield<SEXP> s(Rf_cons(CAR(p), R_NilValue));
      SETCDR(q,s);
      q = CDR(q);
      SET_TAG(q, tag);
    }

    p = CDR(p);
  }
  return attribs;
}
