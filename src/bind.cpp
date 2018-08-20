#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/all_na.h>
#include <tools/collapse.h>
#include <tools/pointer_vector.h>
#include <tools/utils.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/Collecter.h>
#include <tools/bad.h>
#include <dplyr/tbl_cpp.h>

using namespace Rcpp;
using namespace dplyr;


// From Rcpp::DataFrame
static
int df_rows_length(SEXP df) {
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

static
R_xlen_t rows_length(SEXP x, bool rowwise) {
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

static
void inner_vector_check(SEXP x, int nrows, int arg) {
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
                _["expected_size"] = nrows, _["actual_size"] = Rf_length(x));
  }
}

static
bool is_non_data_frame_object(SEXP x) {
  if (TYPEOF(x) != VECSXP) return false;
  if (!OBJECT(x)) return false;
  return !Rf_inherits(x, "data.frame");
}

static
void rbind_vector_check(SEXP x, R_xlen_t nrows, int arg) {
  if (!is_vector(x) || is_non_data_frame_object(x)) {
    bad_pos_arg(arg + 1, "must be a data frame or a named atomic vector, not a {type}",
                _["type"] = get_single_class(x));
  }

  if (rows_length(x, true) != nrows) {
    bad_pos_arg(arg + 1, "must be length {expected_size}, not {actual_size}",
                _["expected_size"] = rows_length(x, true), _["actual_size"] = nrows);
  }

  if (vec_names(x) == R_NilValue) {
    bad_pos_arg(arg + 1, "must have names");
  }
}
static
void cbind_vector_check(SEXP x, R_xlen_t nrows, SEXP contr, int arg) {
  if (is_atomic(x) && !has_name_at(contr, arg))
    bad_pos_arg(arg + 1, "must have names");

  const R_xlen_t actual_nrows = rows_length(x, false);
  if (actual_nrows != nrows) {
    bad_pos_arg(arg + 1, "must be length {expected_size}, not {actual_size}",
                _["expected_size"] = nrows, _["actual_size"] = actual_nrows);
  }
}

static
void rbind_type_check(SEXP x, int nrows, int arg) {
  int n = Rf_length(x);
  if (n == 0)
    return;

  rbind_vector_check(x, nrows, arg);

  if (TYPEOF(x) == VECSXP) {
    for (int i = 0; i < n; i++)
      inner_vector_check(VECTOR_ELT(x, i), nrows, i);
  }
}
static
void cbind_type_check(SEXP x, int nrows, SEXP contr, int arg) {
  int n = Rf_length(x);
  if (n == 0)
    return;

  cbind_vector_check(x, nrows, contr, arg);

  if (TYPEOF(x) == VECSXP) {
    if (OBJECT(x) && !Rf_inherits(x, "data.frame")) {
      bad_pos_arg(arg + 1, "must be a data frame or a named atomic vector, not a {type}",
                  _["type"] = get_single_class(x));
    }
    for (int i = 0; i < n; i++)
      inner_vector_check(VECTOR_ELT(x, i), nrows, i);
  }
}

extern "C"
bool dplyr_is_bind_spliceable(SEXP x) {
  if (TYPEOF(x) != VECSXP)
    return false;

  if (OBJECT(x))
    return Rf_inherits(x, "spliced");

  for (R_xlen_t i = 0; i != Rf_xlength(x); ++i) {
    if (is_atomic(VECTOR_ELT(x, i)))
      return false;
  }

  return true;
}

// [[Rcpp::export]]
SEXP flatten_bindable(SEXP x) {
  // FIXME: This is temporary and should be replaced with rlang::flatten_if()
  typedef bool(*is_spliceable_t)(SEXP);
  typedef SEXP(*rlang_squash_if_t)(SEXP, SEXPTYPE, is_spliceable_t, int);

  static rlang_squash_if_t rlang_squash_if = (rlang_squash_if_t)R_GetCCallable("rlang", "rlang_squash_if");

  return rlang_squash_if(x, VECSXP, &dplyr_is_bind_spliceable, 1);
}

List rbind__impl(List dots, const SymbolString& id) {
  int ndata = dots.size();

  LOG_VERBOSE << "binding at most " << ndata << " chunks";

  R_xlen_t n = 0;
  std::vector<SEXP> chunks;
  std::vector<R_xlen_t> df_nrows;
  std::vector<String> dots_names;

  chunks.reserve(ndata);
  df_nrows.reserve(ndata);
  dots_names.reserve(ndata);

  int k = 0;
  for (int i = 0; i < ndata; i++) {
    SEXP obj = dots[i];
    if (Rf_isNull(obj)) continue;
    chunks.push_back(obj);
    R_xlen_t nrows = rows_length(chunks[k], true);
    df_nrows.push_back(nrows);
    n += nrows;
    if (!id.is_empty()) {
      dots_names.push_back(name_at(dots, i));
    }
    k++;
  }
  ndata = chunks.size();
  pointer_vector<Collecter> columns;


  LOG_VERBOSE << "binding " << ndata << " chunks";

  SymbolVector names;

  k = 0;
  for (int i = 0; i < ndata; i++) {
    Rcpp::checkUserInterrupt();

    SEXP df = chunks[i];
    R_xlen_t nrows = df_nrows[i];
    rbind_type_check(df, nrows, i);

    SymbolVector df_names(vec_names(df));
    for (int j = 0; j < Rf_length(df); j++) {

      SEXP source;
      int offset;
      if (TYPEOF(df) == VECSXP) {
        source = VECTOR_ELT(df, j);
        offset = 0;
      } else {
        source = df;
        offset = j;
      }

      SymbolString name = df_names[j];

      Collecter* coll = 0;
      R_xlen_t index = 0;
      for (; index < names.size(); index++) {
        if (name == names[index]) {
          coll = columns[index];
          break;
        }
      }
      if (!coll) {
        coll = collecter(source, n);
        columns.push_back(coll);
        names.push_back(name);
      }
      if (coll->compatible(source)) {
        // if the current source is compatible, collect
        coll->collect(OffsetSlicingIndex(k, nrows), source, offset);
      } else if (coll->can_promote(source)) {
        // setup a new Collecter
        Collecter* new_collecter = promote_collecter(source, n, coll);

        // import data from this chunk
        new_collecter->collect(OffsetSlicingIndex(k, nrows), source, offset);

        // import data from previous collecter
        new_collecter->collect(NaturalSlicingIndex(k), coll->get());

        // dispose the previous collecter and keep the new one.
        delete coll;
        columns[index] = new_collecter;

      } else if (all_na(source)) {
        // do nothing, the collecter already initialized data with the
        // right NA
      } else if (coll->is_logical_all_na()) {
        Collecter* new_collecter = collecter(source, n);
        new_collecter->collect(OffsetSlicingIndex(k, nrows), source, offset);
        delete coll;
        columns[index] = new_collecter;
      } else {
        bad_col(SymbolString(name), "can't be converted from {source_type} to {target_type}",
                _["source_type"] = coll->describe(), _["target_type"] = get_single_class(source));
      }

    }

    k += nrows;
  }

  int nc = columns.size();

  LOG_VERBOSE << "result has " << nc << " columns";

  int has_id = id.is_empty() ? 0 : 1;

  List out(no_init(nc + has_id));
  SymbolVector out_names(no_init(nc + has_id));
  for (int i = 0; i < nc; i++) {
    out[i + has_id] = columns[i]->get();
    out_names.set(i + has_id, names[i]);
  }

  // Add vector of identifiers if .id is supplied
  if (!id.is_empty()) {
    CharacterVector id_col(no_init(n));

    CharacterVector::iterator it = id_col.begin();
    for (int i = 0; i < ndata; ++i) {
      std::fill(it, it + df_nrows[i], dots_names[i]);
      it += df_nrows[i];
    }
    out[0] = id_col;
    out_names.set(0, id);
  }
  out.attr("names") = out_names;
  set_rownames(out, n);

  LOG_VERBOSE << "result has " << n << " rows";

  // infer the classes group info from the first (#1692)
  if (ndata) {
    SEXP first = chunks[0];
    if (Rf_inherits(first, "data.frame")) {
      set_class(out, get_class(first));
      if (is<GroupedDataFrame>(first)) {
        out = GroupedDataFrame(out, GroupedDataFrame(first)).data();
      }
    } else {
      set_class(out, classes_not_grouped());
    }
  } else {
    set_class(out, classes_not_grouped());
  }

  return out;
}

// [[Rcpp::export]]
List bind_rows_(List dots, SEXP id) {
  LOG_VERBOSE;

  if (Rf_isNull(id))
    return rbind__impl(dots, SymbolString());
  else
    return rbind__impl(dots, SymbolString(Rcpp::as<String>(id)));
}

// [[Rcpp::export]]
List cbind_all(List dots) {
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
    return DataFrame();

  SEXP first = dots[first_i];
  const R_xlen_t nrows = rows_length(first, false);
  cbind_type_check(first, nrows, dots, 0);

  R_xlen_t nv = cols_length(first);

  for (int i = first_i + 1; i < n_dots; i++) {
    SEXP current = dots[i];
    if (Rf_isNull(current))
      continue;

    cbind_type_check(current, nrows, dots, i);
    nv += cols_length(current);
  }

  // collect columns
  List out(nv);
  CharacterVector out_names(nv);

  // Can't use CharacterVector because the result might be R_NilValue
  RObject dots_names = vec_names(dots);

  // then do the subsequent dfs
  for (int i = first_i, k = 0; i < n_dots; i++) {
    SEXP current = dots[i];
    if (Rf_isNull(current))
      continue;

    if (TYPEOF(current) == VECSXP) {
      CharacterVector current_names = vec_names_or_empty(current);

      int nc = Rf_length(current);
      for (int j = 0; j < nc; j++, k++) {
        out[k] = shared_SEXP(VECTOR_ELT(current, j));
        out_names[k] = current_names[j];
      }
    } else {
      out[k] = current;
      out_names[k] = STRING_ELT(dots_names, i);
      k++;
    }

    Rcpp::checkUserInterrupt();
  }

  // infer the classes and extra info (groups, etc ) from the first (#1692)
  if (Rf_inherits(first, "data.frame")) {
    copy_most_attributes(out, first);
  } else {
    set_class(out, classes_not_grouped());
  }

  out.names() = out_names;
  set_rownames(out, nrows);

  return out;
}

// [[Rcpp::export]]
SEXP combine_all(List data) {
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
  if (i == nv) return LogicalVector();

  // collect
  boost::scoped_ptr<Collecter> coll(collecter(data[i], n));
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
      Collecter* new_coll = promote_collecter(current, n, coll.get());
      new_coll->collect(OffsetSlicingIndex(k, n_current), current);
      new_coll->collect(NaturalSlicingIndex(k), coll->get());
      coll.reset(new_coll);
    } else {
      bad_pos_arg(i + 1, "can't be converted from {source_type} to {target_type}",
                  _["source_type"] = get_single_class(current), _["target_type"] = get_single_class(coll->get()));
    }
    k += n_current;
  }

  return coll->get();
}
