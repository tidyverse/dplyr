#include <Rcpp.h>

#include "dplyr/symbols.h"

namespace dplyr {

SEXP get_classes_vctrs_list_of() {
  static Rcpp::CharacterVector klasses(3);
  klasses[0] = "vctrs_list_of";
  klasses[1] = "vctrs_vctr";
  klasses[2] = "list";
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
  ExpanderCollecter(int nvars_, int new_size_, const Rcpp::List& old_rows_) :
    nvars(nvars_),
    old_rows(old_rows_),
    new_size(new_size_),
    new_indices(nvars),
    new_rows(new_size),

    leaf_index(0),
    vec_new_indices(nvars)
  {
    Rf_classgets(new_rows, dplyr::vectors::classes_vctrs_list_of);
    Rf_setAttrib(new_rows, dplyr::symbols::ptype, dplyr::vectors::empty_int_vector);

    for (int i = 0; i < nvars; i++) {
      new_indices[i] = Rf_allocVector(INTSXP, new_size);
      vec_new_indices[i] = INTEGER(new_indices[i]);
    }
  }

  ExpanderResult collect_leaf(int start, int end, int index) {
    if (start == end) {
      new_rows[leaf_index++] = Rf_allocVector(INTSXP, 0);
    } else {
      new_rows[leaf_index++] = old_rows[start];
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


  const Rcpp::List& get_new_rows() const {
    return new_rows;
  }

  const Rcpp::List& get_new_indices() const {
    return new_indices;
  }

private:
  int nvars;
  const Rcpp::List& old_rows;
  int new_size;
  Rcpp::List new_indices;
  Rcpp::List new_rows;

  int leaf_index;

  std::vector<int*> vec_new_indices;

  void fill_indices(int depth, int start, int end, int index) {
    std::fill(vec_new_indices[depth] + start, vec_new_indices[depth] + end, index);
  }

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

// [[Rcpp::export(rng = false)]]
Rcpp::List expand_groups(Rcpp::DataFrame old_groups, Rcpp::List positions, int nr) {
  int nvars = old_groups.size() - 1;

  SEXP names = Rf_getAttrib(old_groups, R_NamesSymbol);
  Rcpp::List old_rows(old_groups[nvars]);
  std::vector<SEXP> vec_data(nvars);
  std::vector<int*> vec_positions(nvars);
  for (int i = 0; i < nvars; i++) {
    vec_data[i] = old_groups[i];
    vec_positions[i] = INTEGER(VECTOR_ELT(positions, i));
  }

  Expander* exp = expander(vec_data, vec_positions, 0, NA_INTEGER, 0, nr);
  ExpanderCollecter results(nvars, exp->size(), old_rows);
  exp->collect(results, 0);
  Rcpp::List out = Rcpp::List::create(
                     results.get_new_indices(),
                     results.get_new_rows()
                   );
  delete exp;

  return out;
}


// [[Rcpp::export(rng = false)]]
SEXP filter_update_rows(int n_rows, SEXP group_indices, SEXP keep, SEXP new_rows_sizes) {
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
