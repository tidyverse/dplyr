#include <dplyr.h>

using namespace Rcpp;
using namespace dplyr;

// [[Rcpp::export]]
void assert_all_white_list(const DataFrame& data) {
  // checking variables are on the white list
  int nc = data.size();
  for (int i=0; i<nc; i++) {
    if (!white_list(data[i])) {
      CharacterVector names = data.names();
      String name_i = names[i];
      SEXP v = data[i];

      SEXP klass = Rf_getAttrib(v, R_ClassSymbol);
      if (!Rf_isNull(klass)) {
        stop("column '%s' has unsupported class : %s",
             name_i.get_cstring() , get_single_class(v));
      }
      else {
        stop("column '%s' has unsupported type : %s",
             name_i.get_cstring() , Rf_type2char(TYPEOF(v)));
      }

    }
  }
}

// [[Rcpp::export]]
SEXP shallow_copy(const List& data) {
  int n = data.size();
  List out(n);
  for (int i=0; i<n; i++) {
    out[i] = shared_SEXP(data[i]);
  }
  copy_attributes(out, data);
  return out;
}

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
