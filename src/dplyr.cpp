#define R_NOREMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <algorithm>
#include <vector>
#include <string>

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

SEXP get_classes_tbl_df() {
  SEXP klasses = Rf_allocVector(STRSXP, 3);
  R_PreserveObject(klasses);
  SET_STRING_ELT(klasses, 0, Rf_mkChar("tbl_df"));
  SET_STRING_ELT(klasses, 1, Rf_mkChar("tbl"));
  SET_STRING_ELT(klasses, 2, Rf_mkChar("data.frame"));
  return klasses;
}

SEXP get_empty_int_vector() {
  SEXP x = Rf_allocVector(INTSXP, 0);
  R_PreserveObject(x);
  return x;
}

SEXP symbols::ptype = Rf_install("ptype");
SEXP symbols::levels = Rf_install("levels");
SEXP symbols::groups = Rf_install("groups");
SEXP symbols::vars = Rf_install("vars");
SEXP symbols::current_group = Rf_install("current_group");
SEXP symbols::rows = Rf_install("rows");
SEXP symbols::dot_dot_group_size = Rf_install("..group_size");
SEXP symbols::dot_dot_group_number = Rf_install("..group_number");
SEXP symbols::mask = Rf_install("mask");
SEXP symbols::caller = Rf_install("caller");

SEXP vectors::classes_vctrs_list_of = get_classes_vctrs_list_of();
SEXP vectors::classes_tbl_df = get_classes_tbl_df();
SEXP vectors::empty_int_vector = get_empty_int_vector();

} // dplyr

namespace rlang {

// *INDENT-OFF*
struct rlang_api_ptrs_t {
  SEXP (*eval_tidy)(SEXP expr, SEXP data, SEXP env);

  rlang_api_ptrs_t() {
    eval_tidy =         (SEXP (*)(SEXP, SEXP, SEXP)) R_GetCCallable("rlang", "rlang_eval_tidy");
  }
};
// *INDENT-ON*

const rlang_api_ptrs_t& rlang_api() {
  static rlang_api_ptrs_t ptrs;
  return ptrs;
}

inline SEXP eval_tidy(SEXP expr, SEXP data, SEXP env) {
  return rlang_api().eval_tidy(expr, data, env);
}

}

namespace vctrs {

// *INDENT-OFF*
struct vctrs_api_ptrs_t {
  bool (*vec_is_vector)(SEXP x);
  R_len_t (*short_vec_size)(SEXP x);

  vctrs_api_ptrs_t() {
    vec_is_vector =         (bool (*)(SEXP)) R_GetCCallable("vctrs", "vec_is_vector");
    short_vec_size  =         (R_len_t (*)(SEXP)) R_GetCCallable("vctrs", "short_vec_size");
  }
};
// *INDENT-ON*

const vctrs_api_ptrs_t& vctrs_api() {
  static vctrs_api_ptrs_t ptrs;
  return ptrs;
}

inline bool vec_is_vector(SEXP x) {
  return vctrs_api().vec_is_vector(x);
}

inline R_len_t short_vec_size(SEXP x) {
  return vctrs_api().short_vec_size(x);
}

}


// support for expand_groups()
class ExpanderCollecter;

struct ExpanderResult {
  ExpanderResult(R_xlen_t start_, R_xlen_t end_, R_xlen_t index_) :
    start(start_),
    end(end_),
    index(index_)
  {}

  R_xlen_t start;
  R_xlen_t end;
  R_xlen_t index;

  inline R_xlen_t size() const {
    return end - start;
  }
};

class Expander {
public:
  virtual ~Expander() {};
  virtual R_xlen_t size() const = 0;
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

  ExpanderResult collect_leaf(R_xlen_t start, R_xlen_t end, R_xlen_t index) {
    if (start == end) {
      SET_VECTOR_ELT(new_rows, leaf_index++, dplyr::vectors::empty_int_vector);
    } else {
      SET_VECTOR_ELT(new_rows, leaf_index++, VECTOR_ELT(old_rows, start));
    }

    return ExpanderResult(leaf_index - 1, leaf_index, index);
  }

  ExpanderResult collect_node(int depth, R_xlen_t index, const std::vector<Expander*>& expanders) {
    int n = expanders.size();
    if (n == 0) {
      return ExpanderResult(NA_INTEGER, NA_INTEGER, index);
    }

    R_xlen_t nr = 0;

    ExpanderResult first = expanders[0]->collect(*this, depth + 1);
    R_xlen_t start = first.start;
    R_xlen_t end = first.end;
    fill_indices(depth, start, end, first.index);

    nr += first.size();

    for (R_xlen_t i = 1; i < n; i++) {
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
  R_xlen_t new_size;
  SEXP new_indices;
  SEXP new_rows;
  int leaf_index;

  std::vector<int*> vec_new_indices;

  void fill_indices(int depth, R_xlen_t start, R_xlen_t end, R_xlen_t index) {
    std::fill(vec_new_indices[depth] + start, vec_new_indices[depth] + end, index);
  }

  ExpanderCollecter(const ExpanderCollecter&);
};


Expander* expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, R_xlen_t index, R_xlen_t start, R_xlen_t end);

inline R_xlen_t expanders_size(const std::vector<Expander*> expanders) {
  R_xlen_t n = 0;
  for (int i = 0; i < expanders.size(); i++) {
    n += expanders[i]->size();
  }
  return n;
}

class FactorExpander : public Expander {
public:
  FactorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, R_xlen_t index_, R_xlen_t start_, R_xlen_t end_) :
    data(data_),
    positions(positions_),
    index(index_),
    start(start_),
    end(end_)
  {
    SEXP fac = data[depth_];
    SEXP levels = Rf_getAttrib(fac, dplyr::symbols::levels);
    R_xlen_t n_levels = XLENGTH(levels);

    expanders.resize(n_levels);

    int* fac_pos = positions[depth_];

    // for each level, setup an expander for `depth + 1`
    R_xlen_t j = start;
    for (R_xlen_t i = 0; i < n_levels; i++) {
      R_xlen_t start_i = j;
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

  virtual R_xlen_t size() const {
    return expanders_size(expanders);
  }

  ExpanderResult collect(ExpanderCollecter& results, int depth) const {
    return results.collect_node(depth, index, expanders);
  }

private:
  const std::vector<SEXP>& data;
  const std::vector<int*>& positions;
  R_xlen_t index;
  R_xlen_t start;
  R_xlen_t end;

  std::vector<Expander*> expanders;
};

class VectorExpander : public Expander {
public:
  VectorExpander(const std::vector<SEXP>& data_, const std::vector<int*>& positions_, int depth_, R_xlen_t index_, R_xlen_t start, R_xlen_t end) :
    index(index_)
  {
    // edge case no data, we need a fake expander with NA index
    if (start == end) {
      expanders.push_back(expander(data_, positions_, depth_ + 1, NA_INTEGER, start, end));
    } else {
      int* vec_pos = positions_[depth_];

      for (R_xlen_t j = start; j < end;) {
        R_xlen_t current = vec_pos[j];
        R_xlen_t start_idx = j;
        while (j < end && vec_pos[++j] == current);
        expanders.push_back(expander(data_, positions_, depth_ + 1, current, start_idx, j));
      }
    }

  }
  ~VectorExpander() {
    for (int i = expanders.size() - 1; i >= 0; i--) delete expanders[i];
  }

  virtual R_xlen_t size() const {
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

  virtual R_xlen_t size() const {
    return 1;
  }

  ExpanderResult collect(ExpanderCollecter& results, int depth) const {
    return results.collect_leaf(start, end, index);
  }

private:
  R_xlen_t index;
  R_xlen_t start;
  R_xlen_t end;
};

Expander* expander(const std::vector<SEXP>& data, const std::vector<int*>& positions, int depth, R_xlen_t index, R_xlen_t start, R_xlen_t end) {
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
  R_xlen_t nvars = XLENGTH(old_groups) - 1;

  SEXP old_rows = VECTOR_ELT(old_groups, nvars);
  std::vector<SEXP> vec_data(nvars);
  std::vector<int*> vec_positions(nvars);
  for (R_xlen_t i = 0; i < nvars; i++) {
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
  R_xlen_t j = 1;
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

SEXP dplyr_mask_eval_all(SEXP quo, SEXP env_private, SEXP env_context) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(XLENGTH(VECTOR_ELT(rows, i))), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SET_VECTOR_ELT(chunks, i, rlang::eval_tidy(quo, mask, caller));

    UNPROTECT(1);
  }

  UNPROTECT(4);

  return chunks;
}

SEXP dplyr_mask_eval_all_summarise(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(rlang::eval_tidy(quo, mask, caller));
    if (!vctrs::vec_is_vector(result_i)) {
      if (!Rf_isNull(dots_names)) {
        SEXP name = STRING_ELT(dots_names, i);
        if (XLENGTH(name) > 0) {
          Rf_errorcall(R_NilValue, "Unsupported type for result `%s`", CHAR(name));
        }
      }
      int i = INTEGER(sexp_i)[0];
      Rf_errorcall(R_NilValue, "Unsupported type at index %d", i);
    }

    SET_VECTOR_ELT(chunks, i, result_i);

    UNPROTECT(2);
  }

  UNPROTECT(4);
  return chunks;
}

SEXP dplyr_mask_eval_all_mutate(SEXP quo, SEXP env_private, SEXP env_context, SEXP dots_names, SEXP sexp_i) {
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 2));
  SEXP chunks = PROTECT(Rf_allocVector(VECSXP, ngroups));
  bool seen_vec = false;
  bool needs_recycle = false;

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(rlang::eval_tidy(quo, mask, caller));
    if (Rf_isNull(result_i)) {
      if (seen_vec) {
        // the current chunk is NULL but there were some non NULL
        // chunks, so this is an error
        Rf_errorcall(R_NilValue, "incompatible results for mutate(), some results are NULL");
      } else {
        UNPROTECT(2);
        continue;
      }
    } else {
      seen_vec = true;
    }

    if (!vctrs::vec_is_vector(result_i)) {
      if (!Rf_isNull(dots_names)) {
        SEXP name = STRING_ELT(dots_names, i);
        if (XLENGTH(name) > 0) {
          Rf_errorcall(R_NilValue, "Unsupported type for result `%s`", CHAR(name));
        }
      }
      int i = INTEGER(sexp_i)[0];
      Rf_errorcall(R_NilValue, "Unsupported type at index %d", i);
    }

    if (!needs_recycle && vctrs::short_vec_size(result_i) != n_i) {
      needs_recycle = true;
    }

    SET_VECTOR_ELT(chunks, i, result_i);

    UNPROTECT(2);
  }


  // there was only NULL results
  if (ngroups > 0 && !seen_vec) {
    chunks = R_NilValue;
  }
  SET_VECTOR_ELT(res, 0, chunks);
  SET_VECTOR_ELT(res, 1, Rf_ScalarLogical(needs_recycle));

  UNPROTECT(5);

  return res;
}

bool all_lgl_columns(SEXP data) {
  R_xlen_t nc = XLENGTH(data);

  for (R_xlen_t i = 0; i < nc; i++) {
    if (TYPEOF(VECTOR_ELT(data, i)) != LGLSXP) return false;
  }

  return true;
}

void reduce_lgl(SEXP reduced, SEXP x, int n) {
  R_xlen_t nres = XLENGTH(x);
  int* p_reduced = LOGICAL(reduced);
  if (nres == 1) {
    if (LOGICAL(x)[0] != TRUE) {
      for (R_xlen_t i = 0; i < n; i++, ++p_reduced) {
        *p_reduced = FALSE;
      }
    }
  } else {
    int* p_x = LOGICAL(x);
    for (R_xlen_t i = 0; i < n; i++, ++p_reduced, ++p_x) {
      *p_reduced = *p_reduced == TRUE && *p_x == TRUE ;
    }
  }
}

SEXP eval_filter_one(SEXP quos, SEXP mask, SEXP caller, R_xlen_t nquos, R_xlen_t n, R_xlen_t group_index, SEXP full_data) {
  // then reduce to a single logical vector of size n
  SEXP reduced = PROTECT(Rf_allocVector(LGLSXP, n));
  bool is_grouped = Rf_inherits(full_data, "grouped_df");

  // init with TRUE
  int* p_reduced = LOGICAL(reduced);
  for (R_xlen_t i = 0; i < n ; i++, ++p_reduced) {
    *p_reduced = TRUE;
  }

  // reduce
  for (R_xlen_t i=0; i < nquos; i++) {
    SEXP res = PROTECT(rlang::eval_tidy(VECTOR_ELT(quos, i), mask, caller));

    if (TYPEOF(res) == LGLSXP) {
      R_xlen_t nres = XLENGTH(res);
      if (nres != n && nres != 1) {
        if (is_grouped) {
          Rf_errorcall(R_NilValue,
            "`filter()` argument #%d is incorrect\n"
            "✖ It must be size %d or 1, not size %d\n"
            "ℹ The error occured in group %d",
            i + 1, n, nres, group_index + 1
          );
        } else {
          Rf_errorcall(R_NilValue,
            "`filter()` argument #%d is incorrect\n"
            "✖ It must be of size %d or 1, not size %d\n",
            i + 1, n, nres
          );
        }
      }
      reduce_lgl(reduced, res, n);
    } else if(Rf_inherits(res, "data.frame")) {
      R_xlen_t ncol = XLENGTH(res);
      if (ncol == 0) continue;

      // reducing each column
      for (R_xlen_t j=0; j<ncol; j++) {
        SEXP res_j = VECTOR_ELT(res, j);

        // we can only reduce logical columns
        if (TYPEOF(res_j) != LGLSXP) {
          SEXP colnames = PROTECT(Rf_getAttrib(res, R_NamesSymbol));
          if (is_grouped) {
            Rf_errorcall(R_NilValue,
              "`filter() expression #%d / column #%d (%s) is incorrect\n"
              "✖ It must be a logical vector, not a %s\n"
              "ℹ The error occured in group %d",
              i+1, j+1, CHAR(STRING_ELT(colnames, j)),
              Rf_type2char(TYPEOF(res_j)),
              group_index + 1
            );
          } else {
            Rf_errorcall(R_NilValue,
              "`filter() expression #%d / column #%d (%s) is incorrect\n"
              "✖ It must be a logical vector, not a %s",
              i+1, j+1, CHAR(STRING_ELT(colnames, j)),
              Rf_type2char(TYPEOF(res_j))
            );
          }
          UNPROTECT(1);
        }

        // ... and the size must be either 1 or n (tidy recycling rules)
        R_xlen_t nres_j = XLENGTH(res_j);
        if (nres_j != n && nres_j != 1) {
          if (is_grouped) {
            Rf_errorcall(R_NilValue,
              "`filter()` argument #%d is incorrect\n"
              "✖ It must be size %d or 1, not size %d\n"
              "ℹ The error occured in group %d",
              i + 1, n, nres_j, group_index + 1
            );
          } else {
            Rf_errorcall(R_NilValue,
              "`filter()` argument #%d is incorrect\n"
              "✖ It must be of size %d or 1, not size %d\n",
              i + 1, n, nres_j
            );
          }
        }

        // all good, we can reduce that column
        reduce_lgl(reduced, res_j, n);

      }
    } else {
      if (is_grouped) {
        Rf_errorcall(R_NilValue,
          "`filter()` argument #%d is incorrect\n"
          "✖ It must a logical vector, not a %s\n"
          "ℹ The error occured in group %d",
          i + 1,
          Rf_type2char(TYPEOF(res)),
          group_index + 1
        );
      } else {
        Rf_errorcall(R_NilValue,
          "`filter()` argument #%d is incorrect\n"
          "✖ It must a logical vector, not a %s",
          i + 1,
          Rf_type2char(TYPEOF(res))
        );
      }
    }
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return reduced;
}

SEXP dplyr_mask_eval_all_filter(SEXP quos, SEXP env_private, SEXP env_context, SEXP s_n, SEXP full_data) {
  R_xlen_t nquos = XLENGTH(quos);
  SEXP rows = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::rows));
  R_xlen_t ngroups = XLENGTH(rows);

  SEXP mask = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::mask));
  SEXP caller = PROTECT(Rf_findVarInFrame(env_private, dplyr::symbols::caller));

  R_xlen_t n = Rf_asInteger(s_n);
  SEXP keep = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_keep = LOGICAL(keep);
  SEXP new_group_sizes = PROTECT(Rf_allocVector(INTSXP, ngroups));
  int* p_new_group_sizes = INTEGER(new_group_sizes);

  SEXP group_indices = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_group_indices = INTEGER(group_indices);

  SEXP res = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(res, 0, keep);
  SET_VECTOR_ELT(res, 1, new_group_sizes);
  SET_VECTOR_ELT(res, 2, group_indices);

  for (R_xlen_t i = 0; i < ngroups; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    SEXP current_group = PROTECT(Rf_ScalarInteger(i + 1));
    Rf_defineVar(dplyr::symbols::current_group, current_group, env_private);
    Rf_defineVar(dplyr::symbols::dot_dot_group_size, Rf_ScalarInteger(n_i), env_context);
    Rf_defineVar(dplyr::symbols::dot_dot_group_number, current_group, env_context);

    SEXP result_i = PROTECT(eval_filter_one(quos, mask, caller, nquos, n_i, i, full_data));

    // sprinkle back to overall logical vector
    int* p_rows_i = INTEGER(rows_i);
    int* p_result_i = LOGICAL(result_i);
    int nkeep = 0;
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i, ++p_result_i) {
      p_keep[*p_rows_i - 1] = *p_result_i == TRUE;
      p_group_indices[*p_rows_i - 1] = i + 1;
      nkeep += (*p_result_i == TRUE);
    }
    p_new_group_sizes[i] = nkeep;

    UNPROTECT(2);
  }

  UNPROTECT(7);

  return res;
}

SEXP dplyr_vec_sizes(SEXP chunks) {
  R_xlen_t n = XLENGTH(chunks);
  SEXP res = PROTECT(Rf_allocVector(INTSXP, n));
  int* p_res = INTEGER(res);

  for (R_xlen_t i = 0; i < n; i++, ++p_res) {
    *p_res = vctrs::short_vec_size(VECTOR_ELT(chunks, i));
  }

  UNPROTECT(1);
  return res;
}

SEXP dplyr_validate_summarise_sizes(SEXP size, SEXP chunks) {
  R_xlen_t nchunks = XLENGTH(chunks);

  if (XLENGTH(size) == 1 && INTEGER(size)[0] == 1) {
    // we might not have to allocate the vector of sizes if
    // all the chunks are of size 1

    R_xlen_t i = 0;
    for (; i < nchunks; i++) {
      if (vctrs::short_vec_size(VECTOR_ELT(chunks, i)) != 1) {
        break;
      }
    }

    if (i == nchunks) {
      // we can just return the input size
      return size;
    }

    // we need to return a vector to track the new sizes
    size = PROTECT(Rf_allocVector(INTSXP, nchunks));
    int* p_size = INTEGER(size);

    // until i, all sizes are 1
    for (R_xlen_t j = 0; j < i; j++, ++p_size) {
      *p_size = 1;
    }

    // then finish with i
    for (; i < nchunks; i++, ++p_size) {
      *p_size = vctrs::short_vec_size(VECTOR_ELT(chunks, i));
    }
    UNPROTECT(1);
    return size;
  } else {
    // size is already a vector, we need to check if the sizes of chunks
    // matches
    int* p_size = INTEGER(size);
    for (R_xlen_t i = 0; i < nchunks; i++, ++p_size) {
      if (*p_size != vctrs::short_vec_size(VECTOR_ELT(chunks, i))) {
        Rf_errorcall(R_NilValue, "Result does not respect vec_size() == .size");
      }
    }
    return size;
  }
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
    for (R_xlen_t i = 0; i < n; ++i, ++p_x, ++p_out) {
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
  R_xlen_t i = 0 ;
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
  for (R_xlen_t i = 1; i < n; i++, ++p_x, ++p_out) {
    sum += *p_x;
    *p_out = sum / (i + 1.0);
  }

  UNPROTECT(1);
  return out;
}

SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_nr_df, SEXP s_check_bounds) {
  if (!Rf_inherits(df, "grouped_df")) {
    return Rf_mkString("not a `grouped_df` object");
  }

  SEXP vars = Rf_getAttrib(df, dplyr::symbols::vars);
  SEXP groups = Rf_getAttrib(df, dplyr::symbols::groups);

  if (!Rf_isNull(vars) && Rf_isNull(groups)) {
    return Rf_mkString("Corrupt grouped_df data using the old format");
  }

  if (!Rf_inherits(groups, "data.frame") || XLENGTH(groups) < 1) {
    return Rf_mkString("The `groups` attribute is not a data frame with its last column called `.rows`");
  }

  SEXP groups_names = Rf_getAttrib(groups, R_NamesSymbol);
  if (Rf_isNull(groups_names) || TYPEOF(groups_names) != STRSXP || ::strcmp(CHAR(STRING_ELT(groups_names, XLENGTH(groups_names) - 1)), ".rows")) {
    return Rf_mkString("The `groups` attribute is not a data frame with its last column called `.rows`");
  }

  SEXP dot_rows = VECTOR_ELT(groups, XLENGTH(groups) - 1);
  if (TYPEOF(dot_rows) != VECSXP) {
    return Rf_mkString("The `groups` attribute is not a data frame with its last column called `.rows`");
  }

  R_xlen_t nr = XLENGTH(dot_rows);
  for (R_xlen_t i = 0; i < nr; i++) {
    SEXP rows_i = VECTOR_ELT(dot_rows, i);
    if (TYPEOF(rows_i) != INTSXP) {
      return Rf_mkString("`.rows` column is not a list of one-based integer vectors");
    }
  }

  if (LOGICAL(s_check_bounds)[0]) {
    R_xlen_t nr_df = TYPEOF(s_nr_df) == INTSXP ? (R_xlen_t)(INTEGER(s_nr_df)[0]) : (R_xlen_t)(REAL(s_nr_df)[0]);
    for (R_xlen_t i = 0; i < nr; i++) {
      SEXP rows_i = VECTOR_ELT(dot_rows, i);
      R_xlen_t n_i = XLENGTH(rows_i);
      int* p_rows_i = INTEGER(rows_i);
      for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
        if (*p_rows_i < 1 || *p_rows_i > nr_df) {
          return Rf_mkString("out of bounds indices");
        }
      }
    }

  }

  return R_NilValue;
}

SEXP dplyr_group_keys_impl(SEXP data) {
  SEXP keys;
  SEXP keys_names;
  SEXP groups = Rf_getAttrib(data, dplyr::symbols::groups);

  R_xlen_t nr;

  if (Rf_isNull(groups)) {
    nr = 1;
    keys_names = PROTECT(Rf_allocVector(STRSXP, 0));
    keys = PROTECT(Rf_allocVector(VECSXP, 0));
  } else {
    R_xlen_t nc = XLENGTH(groups) - 1;
    nr = XLENGTH(VECTOR_ELT(groups, nc));

    SEXP groups_names = Rf_getAttrib(groups, R_NamesSymbol);

    keys = PROTECT(Rf_allocVector(VECSXP, nc));
    keys_names = PROTECT(Rf_allocVector(STRSXP, nc));
    for (R_xlen_t i = 0; i < nc; i++) {
      SET_VECTOR_ELT(keys, i, VECTOR_ELT(groups, i));
      SET_STRING_ELT(keys_names, i, STRING_ELT(groups_names, i));
    }
  }

  SEXP rn = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(rn)[0] = NA_INTEGER;
  INTEGER(rn)[1] = -nr;
  Rf_setAttrib(keys, R_RowNamesSymbol, rn);

  Rf_setAttrib(keys, R_NamesSymbol, keys_names);
  Rf_setAttrib(keys, R_ClassSymbol, dplyr::vectors::classes_tbl_df);

  UNPROTECT(3);
  return keys;
}

SEXP dplyr_group_indices(SEXP data, SEXP s_nr) {
  SEXP groups = Rf_getAttrib(data, dplyr::symbols::groups);
  SEXP rows = VECTOR_ELT(groups, XLENGTH(groups) - 1);
  R_xlen_t nr = INTEGER(s_nr)[0];
  R_xlen_t ng = XLENGTH(rows);

  SEXP indices = PROTECT(Rf_allocVector(INTSXP, nr));
  int* p_indices = INTEGER(indices);
  for (R_xlen_t i = 0; i < ng; i++) {
    SEXP rows_i = VECTOR_ELT(rows, i);
    R_xlen_t n_i = XLENGTH(rows_i);
    int* p_rows_i = INTEGER(rows_i);
    for (R_xlen_t j = 0; j < n_i; j++, ++p_rows_i) {
      p_indices[*p_rows_i - 1] = i + 1;
    }
  }

  UNPROTECT(1);
  return indices;
}

static const R_CallMethodDef CallEntries[] = {
  {"dplyr_expand_groups", (DL_FUNC)& dplyr_expand_groups, 3},
  {"dplyr_filter_update_rows", (DL_FUNC)& dplyr_filter_update_rows, 4},
  {"dplyr_between", (DL_FUNC)& dplyr_between, 3},
  {"dplyr_cumall", (DL_FUNC)& dplyr_cumall, 1},
  {"dplyr_cumany", (DL_FUNC)& dplyr_cumany, 1},
  {"dplyr_cummean", (DL_FUNC)& dplyr_cummean, 1},
  {"dplyr_validate_grouped_df", (DL_FUNC)& dplyr_validate_grouped_df, 3},
  {"dplyr_group_keys_impl", (DL_FUNC)& dplyr_group_keys_impl, 1},

  {"dplyr_mask_eval_all", (DL_FUNC)& dplyr_mask_eval_all, 3},
  {"dplyr_mask_eval_all_summarise", (DL_FUNC)& dplyr_mask_eval_all_summarise, 5},
  {"dplyr_mask_eval_all_mutate", (DL_FUNC)& dplyr_mask_eval_all_mutate, 5},
  {"dplyr_mask_eval_all_filter", (DL_FUNC)& dplyr_mask_eval_all_filter, 5},

  {"dplyr_vec_sizes", (DL_FUNC)& dplyr_vec_sizes, 1},
  {"dplyr_validate_summarise_sizes", (DL_FUNC)& dplyr_validate_summarise_sizes, 2},
  {"dplyr_group_indices", (DL_FUNC)& dplyr_group_indices, 2},

  {NULL, NULL, 0}
};

extern "C" void R_init_dplyr(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
