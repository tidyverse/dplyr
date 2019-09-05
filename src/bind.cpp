#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/all_na.h>
#include <tools/collapse.h>
#include <tools/utils.h>
#include <tools/bad.h>
#include <tools/set_rownames.h>

#include <dplyr/Collecter.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

namespace dplyr {

// From Rcpp::DataFrame
static int df_rows_length(SEXP df) {
  SEXP n = R_NilValue;
  SEXP attrs = ATTRIB(df);
  while (attrs != R_NilValue) {
    if (TAG(attrs) == R_RowNamesSymbol) {
      n = CAR(attrs);
      break;
    }
    attrs = CDR(attrs);
  }

  if (n == R_NilValue)
    return 0;
  else if (TYPEOF(n) == INTSXP && LENGTH(n) == 2 && INTEGER(n)[0] == NA_INTEGER)
    return abs(INTEGER(n)[1]);
  else
    return LENGTH(n);
}

static R_xlen_t rows_length(SEXP x, bool rowwise) {
  if (TYPEOF(x) == VECSXP) {
    if (Rf_inherits(x, "data.frame"))
      return df_rows_length(x);
    else if (Rf_xlength(x) > 0)
      return Rf_xlength(VECTOR_ELT(x, 0));
    else
      return 0;
  } else {
    if (rowwise)
      return 1;
    else
      return Rf_xlength(x);
  }
}
static
R_xlen_t cols_length(SEXP x) {
  if (TYPEOF(x) == VECSXP)
    return Rf_xlength(x);
  else
    return 1;
}

static void inner_vector_check(SEXP x, int nrows, int arg) {
  if (!is_vector(x))
    bad_pos_arg(arg + 1, "is a list, must contain atomic vectors");

  if (OBJECT(x)) {
    if (Rf_inherits(x, "data.frame"))
      bad_pos_arg(arg + 1, "can't be a list containing data frames");
    if (Rf_inherits(x, "POSIXlt"))
      bad_pos_arg(arg + 1, "can't be a list containing POSIXlt values");
  }

  if (Rf_length(x) != nrows) {
    bad_pos_arg(arg + 1, "must be length {expected_size}, not {actual_size}",
                Rcpp::_["expected_size"] = nrows, Rcpp::_["actual_size"] = Rf_length(x));
  }
}

static bool is_non_data_frame_object(SEXP x) {
  if (TYPEOF(x) != VECSXP) return false;
  if (!OBJECT(x)) return false;
  return !Rf_inherits(x, "data.frame");
}

static void rbind_vector_check(SEXP x, R_xlen_t nrows, int arg) {
  if (!is_vector(x) || is_non_data_frame_object(x)) {
    bad_pos_arg(arg + 1, "must be a data frame or a named atomic vector, not a {type}",
                Rcpp::_["type"] = get_single_class(x));
  }

  if (rows_length(x, true) != nrows) {
    bad_pos_arg(arg + 1, "must be length {expected_size}, not {actual_size}",
                Rcpp::_["expected_size"] = rows_length(x, true), Rcpp::_["actual_size"] = nrows);
  }

  if (vec_names(x) == R_NilValue) {
    bad_pos_arg(arg + 1, "must have names");
  }
}
static void cbind_vector_check(SEXP x, R_xlen_t nrows, SEXP contr, int arg) {
  if (is_atomic(x) && !has_name_at(contr, arg))
    bad_pos_arg(arg + 1, "must have names");

  const R_xlen_t actual_nrows = rows_length(x, false);
  if (actual_nrows != nrows) {
    bad_pos_arg(arg + 1, "must be length {expected_size}, not {actual_size}",
                Rcpp::_["expected_size"] = nrows, Rcpp::_["actual_size"] = actual_nrows);
  }
}

static void rbind_type_check(SEXP x, int nrows, int arg) {
  int n = Rf_length(x);
  if (n == 0)
    return;

  rbind_vector_check(x, nrows, arg);

  if (TYPEOF(x) == VECSXP) {
    for (int i = 0; i < n; i++)
      inner_vector_check(VECTOR_ELT(x, i), nrows, i);
  }
}
static void cbind_type_check(SEXP x, int nrows, SEXP contr, int arg) {
  int n = Rf_length(x);
  if (n == 0)
    return;

  cbind_vector_check(x, nrows, contr, arg);

  if (TYPEOF(x) == VECSXP) {
    if (OBJECT(x) && !Rf_inherits(x, "data.frame")) {
      bad_pos_arg(arg + 1, "must be a data frame or a named atomic vector, not a {type}",
                  Rcpp::_["type"] = get_single_class(x));
    }
    for (int i = 0; i < n; i++)
      inner_vector_check(VECTOR_ELT(x, i), nrows, i);
  }
}

}

extern "C" bool dplyr_is_bind_spliceable(SEXP x) {
  if (TYPEOF(x) != VECSXP)
    return false;

  if (Rf_inherits(x, "spliced")) return true;
  if (Rf_inherits(x, "data.frame")) return false;

  for (R_xlen_t i = 0; i != Rf_xlength(x); ++i) {
    if (is_atomic(VECTOR_ELT(x, i)))
      return false;
  }

  return true;
}

// [[Rcpp::export(rng = false)]]
SEXP flatten_bindable(SEXP x) {
  // FIXME: This is temporary and should be replaced with rlang::flatten_if()
  typedef bool(*is_spliceable_t)(SEXP);
  typedef SEXP(*rlang_squash_if_t)(SEXP, SEXPTYPE, is_spliceable_t, int);

  static rlang_squash_if_t rlang_squash_if = (rlang_squash_if_t)R_GetCCallable("rlang", "rlang_squash_if");

  return rlang_squash_if(x, VECSXP, &dplyr_is_bind_spliceable, 1);
}

// [[Rcpp::export(rng = false)]]
void bind_rows_check(Rcpp::List dots) {
  int ndata = dots.size();

  for (int i = 0; i < ndata; i++) {
    SEXP obj = dots[i];
    if (Rf_isNull(obj)) continue;

    R_xlen_t nrows = dplyr::rows_length(obj, true);
    dplyr::rbind_type_check(obj, nrows, i);
  }

}

// [[Rcpp::export(rng = false)]]
SEXP cbind_all(Rcpp::List dots) {
  int n_dots = dots.size();

  // First check that the number of rows is the same based on first
  // nonnull element
  int first_i = -1;
  for (int i = 0; i != n_dots; ++i) {
    if (dots[i] != R_NilValue) {
      first_i = i;
      break;
    }
  }

  if (!n_dots || first_i == -1)
    return Rcpp::DataFrame();

  SEXP first = dots[first_i];
  const R_xlen_t nrows = dplyr::rows_length(first, false);
  dplyr::cbind_type_check(first, nrows, dots, 0);

  R_xlen_t nv = dplyr::cols_length(first);

  for (int i = first_i + 1; i < n_dots; i++) {
    SEXP current = dots[i];
    if (Rf_isNull(current))
      continue;

    dplyr::cbind_type_check(current, nrows, dots, i);
    nv += dplyr::cols_length(current);
  }

  // collect columns
  Rcpp::Shield<SEXP> out(Rf_allocVector(VECSXP, nv));
  Rcpp::Shield<SEXP> out_names(Rf_allocVector(STRSXP, nv));

  // Can't use CharacterVector because the result might be R_NilValue
  Rcpp::RObject dots_names = vec_names(dots);

  // then do the subsequent dfs
  for (int i = first_i, k = 0; i < n_dots; i++) {
    SEXP current = dots[i];
    if (Rf_isNull(current))
      continue;

    if (TYPEOF(current) == VECSXP) {
      Rcpp::Shield<SEXP> current_names(vec_names_or_empty(current));

      int nc = Rf_length(current);
      for (int j = 0; j < nc; j++, k++) {
        SET_VECTOR_ELT(out, k, shared_SEXP(VECTOR_ELT(current, j)));
        SET_STRING_ELT(out_names, k, STRING_ELT(current_names, j));
      }
    } else {
      SET_VECTOR_ELT(out, k, current);
      SET_STRING_ELT(out_names, k, STRING_ELT(dots_names, i));
      k++;
    }

    Rcpp::checkUserInterrupt();
  }

  // infer the classes and extra info (groups, etc ) from the first (#1692)
  if (Rf_inherits(first, "data.frame")) {
    dplyr::copy_most_attributes(out, first);
  } else {
    dplyr::set_class(out, dplyr::NaturalDataFrame::classes());
  }
  Rf_namesgets(out, out_names);
  dplyr::set_rownames(out, nrows);

  return out;
}

// [[Rcpp::export(rng = false)]]
SEXP combine_all(Rcpp::List data) {
  int nv = data.size();

  // get the size of the output
  int n = 0;
  for (int i = 0; i < nv; i++) {
    n += Rf_length(data[i]);
  }

  // go to the first non NULL
  int i = 0;
  for (; i < nv; i++) {
    if (!Rf_isNull(data[i])) break;
  }
  if (i == nv) return Rcpp::LogicalVector();

  // collect
  boost::scoped_ptr<dplyr::Collecter> coll(dplyr::collecter(data[i], n));
  int k = Rf_length(data[i]);
  coll->collect(NaturalSlicingIndex(k), data[i]);
  i++;
  for (; i < nv; i++) {
    SEXP current = data[i];
    if (Rf_isNull(current)) continue;
    int n_current = Rf_length(current);

    if (coll->compatible(current)) {
      coll->collect(OffsetSlicingIndex(k, n_current), current);
    } else if (coll->can_promote(current)) {
      dplyr::Collecter* new_coll = promote_collecter(current, n, coll.get());
      new_coll->collect(OffsetSlicingIndex(k, n_current), current);
      new_coll->collect(NaturalSlicingIndex(k), coll->get());
      coll.reset(new_coll);
    } else {
      dplyr::bad_pos_arg(i + 1, "can't be converted from {source_type} to {target_type}",
                         Rcpp::_["source_type"] = dplyr::get_single_class(current),
                         Rcpp::_["target_type"] = dplyr::get_single_class(coll->get()));
    }
    k += n_current;
  }

  return coll->get();
}
