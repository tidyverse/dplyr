#define R_NOREMAP
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <vector>

#include "dplyr/symbols.h"

namespace dplyr {

SEXP get_classes_vctrs_list_of() {
  SEXP klasses = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(klasses);
  SET_STRING_ELT(klasses, 0, Rf_mkChar("vctrs_list_of"));
  SET_STRING_ELT(klasses, 1, Rf_mkChar("vctrs_vctr"));
  SET_STRING_ELT(klasses, 2, Rf_mkChar("list"));
  return klasses;
}

SEXP get_empty_int_vector() {
  SEXP x = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(x);
  return x;
}

SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::levels = Rf_install("levels");

SEXP vectors::classes_vctrs_list_of = get_classes_vctrs_list_of();
SEXP vectors::empty_int_vector = get_empty_int_vector();

}

// support for expand_groups()
class ExpanderCollecter;

struct ExpanderResult {
  ExpanderResult(int start_, int end_, int index_) :
    start(start_),
    end(end_),
    index(index_)
  {}

  int start;
  int end;
  int index;

  inline int size() const {
    return end - start;
  }
};

class Expander {
public:
  virtual ~Expander() {};
  virtual int size() const = 0;
  virtual ExpanderResult collect(ExpanderCollecter& results, int depth) const = 0;
};

class ExpanderCollecter {
public:
  ExpanderCollecter(int nvars_, SEXP new_indices_, int new_size_, SEXP new_rows_, SEXP old_rows_) :
    nvars(nvars_),
    old_rows(old_rows_),
    new_size(new_size_),
    new_indices(new_indices_),
    new_rows(new_rows_),

    leaf_index(0),
    vec_new_indices(nvars)
  {
    Rf_classgets(new_rows, dplyr::vectors::classes_vctrs_list_of);
    Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

    for (int i = 0; i < nvars; i++) {
      SEXP new_indices_i = Rf_allocVector(INTSXP, new_size);
      SET_VECTOR_ELT(new_indices, i, new_indices_i);
      vec_new_indices[i] = INTEGER(new_indices_i);
    }
  }

  ExpanderResult collect_leaf(int start, int end, int index) {
    if (start == end) {
      SET_VECTOR_ELT(new_rows, leaf_index++, dplyr::vectors::empty_int_vector);
    } else {
      SET_VECTOR_ELT(new_rows, leaf_index++, VECTOR_ELT(old_rows, start));
    }

    return ExpanderResult(leaf_index - 1, leaf_index, index);
  }

  ExpanderResult collect_node(int depth, int index, const std::vector<Expander*>& expanders) {
    int n = expanders.size();
    if (n == 0) {
      return ExpanderResult(NA_INTEGER, NA_INTEGER, index);
    }

    int nr = 0;

    ExpanderResult first = expanders[0]->collect(*this, depth + 1);
    int start = first.start;
    int end = first.end;
    fill_indices(depth, start, end, first.index);

    nr += first.size();

    for (int i = 1; i < n; i++) {
      ExpanderResult exp_i = expanders[i]->collect(*this, depth + 1);
      fill_indices(depth, exp_i.start, exp_i.end, exp_i.index);

      nr += exp_i.size();
      end = exp_i.end;
    }

    return ExpanderResult(start, end, index);
  }

private:
  int nvars;
  SEXP old_rows;
  int new_size;
  SEXP new_indices;
  SEXP new_rows;

  int leaf_index;

  std::vector<int*> vec_new_indices;

  void fill_indices(int depth, int start, int end, int index) {
    std::fill(vec_new_indices[depth] + start, vec_new_indices[depth] + end, index);
  }

  ExpanderCollecter(const ExpanderCollecter&);
};


Expander* expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, int index, int start, int end);

inline int expanders_size(const std::vector<Expander*> expanders) {
  int n = 0;
  for (int i = 0; i < expanders.size(); i++) {
    n += expanders[i]->size();
  }
  return n;
}

class FactorExpander : public Expander {
public:
  FactorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start_, int end_) :
    data(data_),
    positions(positions_),
    index(index_),
    start(start_),
    end(end_)
  {
    SEXP fac = data[depth_];
    SEXP levels = Rf_getAttrib(fac, dplyr::symbols::levels);
    int n_levels = XLENGTH(levels);

    expanders.resize(n_levels);

    int* fac_pos = positions[depth_];

    // for each level, setup an expander for `depth + 1`
    int j = start;
    for (int i = 0; i < n_levels; i++) {
      int start_i = j;
      while (j < end && fac_pos[j] == i + 1) j++;
      expanders[i] = expander(data, positions, depth_ + 1, i + 1, start_i, j);
    }

    // implicit NA
    if (j < end) {
      expanders.push_back(expander(data, positions, depth_ + 1, NA_INTEGER, j, end));
    }
  }
  ~FactorExpander() {
    for (int i = expanders.size() - 1; i >= 0; i--) delete expanders[i];
  }

  virtual int size() const {
    return expanders_size(expanders);
  }

  ExpanderResult collect(ExpanderCollecter& results, int depth) const {
    return results.collect_node(depth, index, expanders);
  }

private:
  const std::vector<SEXP>& data;
  const std::vector<int*>& positions;
  int index;
  int start;
  int end;

  std::vector<Expander*> expanders;
};

class VectorExpander : public Expander {
public:
  VectorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start, int end) :
    index(index_)
  {
    // edge case no data, we need a fake expander with NA index
    if (start == end) {
      expanders.push_back(expander(data_, positions_, depth_ + 1, NA_INTEGER, start, end));
    } else {
      int* vec_pos = positions_[depth_];

      for (int j = start; j < end;) {
        int current = vec_pos[j];
        int start_idx = j;
        while (j < end && vec_pos[++j] == current);
        expanders.push_back(expander(data_, positions_, depth_ + 1, current, start_idx, j));
      }
    }

  }
  ~VectorExpander() {
    for (int i = expanders.size() - 1; i >= 0; i--) delete expanders[i];
  }

  virtual int size() const {
    return expanders_size(expanders);
  }

  ExpanderResult collect(ExpanderCollecter& results, int depth) const {
    return results.collect_node(depth, index, expanders);
  }

private:
  int index;
  std::vector<Expander*> expanders;
};

class LeafExpander : public Expander {
public:
  LeafExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, int index_, int start_, int end_) :
    index(index_),
    start(start_),
    end(end_)
  {}

  ~LeafExpander() {}

  virtual int size() const {
    return 1;
  }

  ExpanderResult collect(ExpanderCollecter& results, int depth) const {
    return results.collect_leaf(start, end, index);
  }

private:
  int index;
  int start;
  int end;
};

Expander* expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, int index, int start, int end) {
  if (depth == positions.size()) {
    return new LeafExpander(data, positions, depth, index, start, end);
  } else if (Rf_isFactor(data[depth])) {
    return new FactorExpander(data, positions, depth, index, start, end);
  } else {
    return new VectorExpander(data, positions, depth, index, start, end);
  }
}

SEXP dplyr_expand_groups(SEXP old_groups, SEXP positions, SEXP s_nr) {
  int nr = INTEGER(s_nr)[0];
  int nvars = XLENGTH(old_groups) - 1;

  SEXP names = Rf_getAttrib(old_groups, R_NamesSymbol);
  SEXP old_rows = VECTOR_ELT(old_groups, nvars);
  std::vector<SEXP> vec_data(nvars);
  std::vector<int*> vec_positions(nvars);
  for (int i = 0; i < nvars; i++) {
    vec_data[i] = VECTOR_ELT(old_groups, i);
    vec_positions[i] = INTEGER(VECTOR_ELT(positions, i));
  }

  Expander* exp = expander(vec_data, vec_positions, 0, NA_INTEGER, 0, nr);
  SEXP new_indices = PROTECT(Rf_allocVector(VECSXP, nvars));
  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, exp->size()));
  ExpanderCollecter results(nvars, new_indices, exp->size(), new_rows, old_rows);
  exp->collect(results, 0);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 2));
  SET_VECTOR_ELT(out, 0, new_indices);
  SET_VECTOR_ELT(out, 1, new_rows);

  delete exp;

  UNPROTECT(3);
  return out;
}


SEXP dplyr_filter_update_rows(SEXP s_n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes) {
  int n_rows = INTEGER(s_n_rows)[0];

  R_xlen_t n_groups = XLENGTH(new_rows_sizes);

  SEXP new_rows = PROTECT(Rf_allocVector(VECSXP, n_groups));
  Rf_setAttrib(new_rows, R_ClassSymbol, dplyr::vectors::classes_vctrs_list_of);
  Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

  // allocate each new_rows element
  int* p_new_rows_sizes = INTEGER(new_rows_sizes);
  std::vector<int> tracks(n_groups);
  std::vector<int*> p_new_rows(n_groups);
  for (R_xlen_t i = 0; i < n_groups; i++) {
    SEXP new_rows_i = Rf_allocVector(INTSXP, p_new_rows_sizes[i]);
    SET_VECTOR_ELT(new_rows, i, new_rows_i);
    p_new_rows[i] = INTEGER(new_rows_i);
  }

  // traverse group_indices and keep to fill new_rows
  int* p_group_indices = INTEGER(group_indices);
  int* p_keep = LOGICAL(keep);
  int j = 1;
  for (R_xlen_t i = 0; i < n_rows; i++) {
    if (p_keep[i] == TRUE) {
      int g = p_group_indices[i];
      int track = tracks[g - 1]++;
      p_new_rows[g - 1][track] = j++;
    }
  }

  UNPROTECT(1);

  return new_rows;
}

// ------- funs

SEXP dplyr_between(SEXP x, SEXP s_left, SEXP s_right) {
  R_xlen_t n = XLENGTH(x);

  double left = REAL(s_left)[0], right = REAL(s_right)[0];
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  // Assume users know what they're doing with date/times. In the future
  // should ensure that left and right are the correct class too.
  if (!Rf_isNull(Rf_getAttrib(x, R_ClassSymbol)) && !Rf_inherits(x, "Date") && !Rf_inherits(x, "POSIXct")) {
    Rf_warningcall(R_NilValue, "between() called on numeric vector with S3 class");
  }

  if (R_IsNA(left) || R_IsNA(right)) {
    std::fill(LOGICAL(out), LOGICAL(out) + n, NA_LOGICAL);
  } else {
    int* p_out = LOGICAL(out);
    double* p_x = REAL(x);
    for (int i = 0; i < n; ++i, ++p_x, ++p_out) {
      *p_out = R_IsNA(*p_x) ? NA_LOGICAL : (*p_x >= left) && (*p_x <= right);
    }
  }

  UNPROTECT(1);
  return out;
}

SEXP dplyr_cumall(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_x = LOGICAL(x);
  int* p_out = LOGICAL(out);

  // set out[i] to TRUE as long as x[i] is TRUE
  R_xlen_t i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == TRUE) {
      *p_out = TRUE;
    } else {
      break;
    }
  }
  if (i != n) {

    // set to NA as long as x[i] is NA or TRUE
    for (; i < n; i++, ++p_x, ++p_out) {
      if (*p_x == FALSE) {
        break;
      }
      *p_out = NA_LOGICAL;
    }

    // set remaining to FALSE
    if (i != n) {
      for (; i < n; i++, ++p_x, ++p_out) {
        *p_out = FALSE;
      }
    }

  }

  UNPROTECT(1);
  return out;
}

SEXP dplyr_cumany(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(LGLSXP, n));

  int* p_x = LOGICAL(x);
  int* p_out = LOGICAL(out);

  // nothing to do as long as x[i] is FALSE
  int i = 0 ;
  for (; i < n; i++, ++p_x, ++p_out) {
    if (*p_x == FALSE) {
      *p_out = FALSE;
    } else {
      break;
    }
  }
  if (i < n) {
    // set to NA as long as x[i] is NA or FALSE
    for (; i < n; i++, ++p_x, ++p_out) {
      if (*p_x == TRUE) {
        break;
      }
      *p_out = NA_LOGICAL;
    }

    if (i < n) {
      // then if we are here, the rest is TRUE
      for (; i < n; i++, ++p_out) {
        *p_out = TRUE;
      }
    }

  }

  UNPROTECT(1);
  return out;

}

SEXP dplyr_cummean(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));

  double* p_out = REAL(out);
  double* p_x = REAL(x);

  double sum = *p_out++ = *p_x;
  for (int i = 1; i < n; i++, ++p_x, ++p_out) {
    sum += *p_x;
    *p_out = sum / (i + 1.0);
  }

  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"dplyr_expand_groups", (DL_FUNC)& dplyr_expand_groups, 3},
  {"dplyr_filter_update_rows", (DL_FUNC)& dplyr_filter_update_rows, 4},
  {"dplyr_between", (DL_FUNC)& dplyr_between, 3},
  {"dplyr_cumall", (DL_FUNC)& dplyr_cumall, 1},
  {"dplyr_cumany", (DL_FUNC)& dplyr_cumany, 1},
  {"dplyr_cummean", (DL_FUNC)& dplyr_cummean, 1},
  {NULL, NULL, 0}
};

extern "C" void R_init_dplyr(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
