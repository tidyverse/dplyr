#include "dplyr.h"
#include <vector>

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

  Rf_namesgets(out, dplyr::vectors::expanded_names);

  UNPROTECT(3);
  return out;
}

SEXP dplyr_validate_grouped_df(SEXP df, SEXP s_check_bounds) {
  if (!Rf_inherits(df, "grouped_df")) {
    return Rf_mkString("not a `grouped_df` object");
  }

  SEXP groups = Rf_getAttrib(df, dplyr::symbols::groups);

  if (Rf_isNull(groups)) {
    SEXP vars = Rf_getAttrib(df, dplyr::symbols::vars);
    if (!Rf_isNull(vars)){
      return Rf_mkString("Corrupt grouped_df data using the old format");
    }
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

  if (LOGICAL(s_check_bounds)[0]) {
    R_xlen_t nr = XLENGTH(dot_rows);
    for (R_xlen_t i = 0; i < nr; i++) {
      SEXP rows_i = VECTOR_ELT(dot_rows, i);
      if (TYPEOF(rows_i) != INTSXP) {
        return Rf_mkString("`.rows` column is not a list of one-based integer vectors");
      }
    }

    R_xlen_t nr_df = vctrs::short_vec_size(df);
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
