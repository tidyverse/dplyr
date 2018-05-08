#include "pch.h"
#include <dplyr/main.h>
#include <dplyr/white_list.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/DataFrameJoinVisitors.h>

#include <dplyr/Order.h>

#include <dplyr/Result/Count.h>

#include <dplyr/train.h>

#include <dplyr/bad.h>
#include <dplyr/tbl_cpp.h>

#include <tools/match.h>
#include <boost/shared_ptr.hpp>
#include <dplyr/default_value.h>

using namespace Rcpp;
using namespace dplyr;

# if __cplusplus >= 201103L
#define MOVE(x) std::move(x)
# else
#define MOVE(x) x
# endif

// [[Rcpp::export]]
IntegerVector grouped_indices_grouped_df_impl(GroupedDataFrame gdf) {
  int n = gdf.nrows();
  IntegerVector res = no_init(n);
  int ngroups = gdf.ngroups();
  GroupedDataFrameIndexIterator it = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++it) {
    const SlicingIndex& index = *it;
    int n_index = index.size();
    for (int j = 0; j < n_index; j++) {
      res[ index[j] ] = i + 1;
    }
  }
  return res;
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp(GroupedDataFrame gdf) {
  return Count().process(gdf);
}

SEXP unique_levels(SEXP f) {
  SEXP labels = Rf_getAttrib(f, R_LevelsSymbol);
  IntegerVector values = seq_len(Rf_length(labels));
  copy_attributes(values, f);
  return values;
}

class IntRange {
public:
  IntRange() : start(-1), size(0) {}

  IntRange(int start_, int size_):
    start(start_), size(size_)
  {}

  void add(const IntRange& other) {
    if (start < 0) {
      start = other.start;
    }
    size += other.size;
  }

  int start;
  int size;
};

class ListCollecter {
public:
  ListCollecter(List& data_): data(data_), index(0) {}

  int collect(const std::vector<int>& indices) {
    data[index] = indices;
    return index++;
  }

private:
  List& data;
  int index;
};

template <int RTYPE>
class CopyVectorVisitor {
public:
  // need to fix it in Rcpp first
  // https://github.com/RcppCore/Rcpp/issues/849
  // typedef typename Rcpp::Vector<RTYPE, NoProtectStorage> Vec;
  typedef typename Rcpp::Vector<RTYPE> Vec;

  CopyVectorVisitor(Vec target_, Vec origin_) :
    target(target_), origin(origin_)
  {}

  virtual void copy(const IntRange& target_range, int idx_origin) {
    std::fill_n(
      target.begin() + target_range.start, target_range.size,
      idx_origin == NA_INTEGER ? default_value<RTYPE>() : origin[idx_origin]
    );
  }

private:
  Vec target;
  Vec origin;
};

inline void copy_visit(const IntRange& target_range, int idx_origin, SEXP target, SEXP origin) {
  switch (TYPEOF(target)) {
  case INTSXP:
    CopyVectorVisitor<INTSXP>(target, origin).copy(target_range, idx_origin);
    break;
  case REALSXP:
    CopyVectorVisitor<REALSXP>(target, origin).copy(target_range, idx_origin);
    break;
  case LGLSXP:
    CopyVectorVisitor<LGLSXP>(target, origin).copy(target_range, idx_origin);
    break;
  case STRSXP:
    CopyVectorVisitor<STRSXP>(target, origin).copy(target_range, idx_origin);
    break;
  case RAWSXP:
    CopyVectorVisitor<RAWSXP>(target, origin).copy(target_range, idx_origin);
    break;
  case CPLXSXP:
    CopyVectorVisitor<CPLXSXP>(target, origin).copy(target_range, idx_origin);
    break;
  }
}

class Slicer {
public:
  virtual ~Slicer() {};
  virtual int size() = 0;
  virtual IntRange make(List& vec_labels, ListCollecter& indices_collecter) = 0;
};
boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_);

class LeafSlicer : public Slicer {
public:
  LeafSlicer(const std::vector<int>& index_range_) : index_range(index_range_) {}

  virtual int size() {
    return 1;
  }

  virtual IntRange make(List& vec_labels, ListCollecter& indices_collecter) {
    return IntRange(indices_collecter.collect(index_range), 1);
  }

  virtual ~LeafSlicer() {};

private:
  const std::vector<int>& index_range;
};

class EchoVector {
public:
  EchoVector(int n_) : n(n_) {}

  inline int operator[](int i) const {
    return i;
  }

  inline int size() const {
    return n;
  }

private:
  int n;
};

class FactorSlicer : public Slicer {
public:
  typedef IntegerVector Factor;

  FactorSlicer(int depth_, const std::vector<int>& index_range, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_) :
    depth(depth_),
    data(data_),
    visitors(visitors_),

    f(data[depth]),
    nlevels(Rf_length(f.attr("levels"))),

    indices(nlevels + 1),
    slicers(nlevels + 1),
    slicer_size(0),
    has_implicit_na(false)
  {
    train(index_range);
  }

  virtual int size() {
    return slicer_size;
  }

  virtual IntRange make(List& vec_labels, ListCollecter& indices_collecter) {
    IntRange labels_range;
    SEXP x = vec_labels[depth];

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_labels, indices_collecter);
      labels_range.add(idx);

      // fill the labels at these indices
      std::fill_n(INTEGER(x) + idx.start, idx.size, i + 1);
    }

    if (has_implicit_na) {
      // collect the indices for the implicit NA pseudo group
      IntRange idx = slicers[nlevels]->make(vec_labels, indices_collecter);
      labels_range.add(idx);

      // fill the labels at these indices
      std::fill_n(INTEGER(x) + idx.start, idx.size, NA_INTEGER);
    }

    return labels_range;
  }

  virtual ~FactorSlicer() {}

private:

  void train(const std::vector<int>& index_range) {

    // special case for depth==0 so that we don't have to build
    // the 0:(n-1) vector indices
    if (depth == 0) {
      train_impl(EchoVector(Rf_length(data[0])));
    } else {
      train_impl(index_range);
    }
    if (!has_implicit_na) {
      indices.pop_back();
      slicers.pop_back();
    }
    // ---- for each level, train child slicers
    int n = nlevels + has_implicit_na;
    for (int i = 0; i < n; i++) {
      slicers[i] = slicer(indices[i], depth + 1, data, visitors);
      slicer_size += slicers[i]->size();
    }

  }

  template <typename Indices>
  void train_impl(const Indices& range) {
    int n = range.size();
    for (int i = 0; i < n; i++) {
      int idx = range[i];
      int value = f[idx];

      if (value == NA_INTEGER) {
        has_implicit_na = true;
        indices[nlevels].push_back(idx);
      } else {
        indices[value - 1].push_back(idx);
      }

    }
  }

  int depth;

  const std::vector<SEXP>& data;
  const DataFrameVisitors& visitors;

  Factor f;
  int nlevels;

  std::vector< std::vector<int> > indices;
  std::vector< boost::shared_ptr<Slicer> > slicers;
  int slicer_size;
  bool has_implicit_na;
};

class VectorSlicer : public Slicer {
private:
  typedef std::pair<int, const std::vector<int>* > IndicesPair;

  class PairCompare {
  public:
    PairCompare(VectorVisitor* v_) : v(v_) {};

    bool operator()(const IndicesPair& x, const IndicesPair& y) {
      return v->less(x.first, y.first);
    }

  private:
    VectorVisitor* v;
  };

public:

  VectorSlicer(int depth_, const std::vector<int>& index_range, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_) :
    depth(depth_),
    // index_range(index_range_),
    data(data_),
    visitors(visitors_),

    visitor(visitors_.get(depth)),
    indices(),
    slicer_size(0)
  {
    train(index_range);
  }

  virtual int size() {
    return slicer_size;
  }

  virtual IntRange make(List& vec_labels, ListCollecter& indices_collecter) {
    IntRange labels_range;
    int nlevels = slicers.size();

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_labels, indices_collecter);
      labels_range.add(idx);

      // fill the labels at these indices
      copy_visit(idx, agents[i], vec_labels[depth], data[depth]);
    }

    return labels_range;
  }

  virtual ~VectorSlicer() {}

private:
  void train(const std::vector<int>& index_range) {
    if (depth == 0) {
      train_impl(EchoVector(Rf_length(data[0])));
    } else {
      train_impl(index_range);
    }

    // ---- for each level, train child slicers
    int n = indices.size();
    slicers.reserve(n);
    for (int i = 0; i < n; i++) {
      slicers.push_back(slicer(indices[i], depth + 1, data, visitors));
      slicer_size += slicers[i]->size();
    }
  }

  template <typename Indices>
  void train_impl(const Indices& index_range) {
    int n = index_range.size();
    if (n == 0) {
      // deal with special case when index_range is empty

      agents.push_back(NA_INTEGER);         // NA is used as a placeholder
      indices.push_back(std::vector<int>()); // empty indices

    } else {
      Map map(visitor, n);
      // train the map
      for (int i = 0; i < n; i++) {
        int idx = index_range[i];
        map[idx].push_back(idx);
      }

      // fill agents and indices
      int nlevels = map.size();

      std::vector<IndicesPair> map_collect;
      for (Map::const_iterator it = map.begin(); it != map.end(); ++it) {
        map_collect.push_back(std::make_pair<int, const std::vector<int>* >(int(it->first), &it->second));
      }
      PairCompare compare(visitors.get(depth));
      std::sort(map_collect.begin(), map_collect.end(), compare);

      // make sure the vectors are not resized
      indices.reserve(nlevels);
      agents.reserve(nlevels);
      slicers.reserve(nlevels);

      // ---- for each case, create indices
      for (int i = 0; i < nlevels; i++) {
        agents.push_back(map_collect[i].first);
        indices.push_back(MOVE(*map_collect[i].second));
      }

    }

  }

  typedef VisitorSetIndexMap<VectorVisitor, std::vector<int> > Map;

  int depth;

  const std::vector<SEXP> data;
  const DataFrameVisitors& visitors;

  VectorVisitor* visitor;

  std::vector< int > agents;
  std::vector< std::vector<int> > indices;
  std::vector< boost::shared_ptr<Slicer> > slicers;
  int slicer_size;
};

boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data, const DataFrameVisitors& visitors) {
  if (depth == data.size()) {
    return boost::shared_ptr<Slicer>(new LeafSlicer(index_range));
  } else {
    SEXP x = data[depth];
    if (Rf_isFactor(x)) {
      return boost::shared_ptr<Slicer>(new FactorSlicer(depth, index_range, data, visitors));
    } else {
      return boost::shared_ptr<Slicer>(new VectorSlicer(depth, index_range, data, visitors));
    }
  }
}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
void build_index_cpp(DataFrame& data) {
  SymbolVector vars(get_vars(data));
  const int nvars = vars.size();

  CharacterVector names = data.names();
  IntegerVector indx = vars.match_in_table(names);
  std::vector<SEXP> visited_data(nvars);
  CharacterVector label_names(nvars);

  for (int i = 0; i < nvars; ++i) {
    int pos = indx[i];
    if (pos == NA_INTEGER) {
      bad_col(vars[i], "is unknown");
    }

    SEXP v = data[pos - 1];
    visited_data[i] = v;
    label_names[i] = names[pos - 1];

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      bad_col(vars[i], "can't be used as a grouping variable because it's a {type}",
              _["type"] = get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);

  boost::shared_ptr<Slicer> s = slicer(std::vector<int>(), 0, visited_data, visitors);

  int ncases = s->size();

  // construct the labels data
  List vec_labels(nvars);
  List indices(ncases);
  ListCollecter indices_collecter(indices);

  for (int i = 0; i < nvars; i++) {
    vec_labels[i] = Rf_allocVector(TYPEOF(visited_data[i]), ncases);
    copy_most_attributes(vec_labels[i], visited_data[i]);
  }

  s->make(vec_labels, indices_collecter);

  // warn about NA in factors
  for (int i = 0; i < nvars; i++) {
    SEXP x = vec_labels[i];
    if (Rf_isFactor(x)) {
      IntegerVector xi(x);
      if (std::find(xi.begin(), xi.end(), NA_INTEGER) < xi.end()) {
        warningcall(R_NilValue, tfm::format("Factor `%s` contains implicit NA, consider using `forcats::fct_explicit_na`", CHAR(label_names[i].get())));
      }
    }
  }

  vec_labels.attr("names") = label_names;
  vec_labels.attr("row.names") = IntegerVector::create(NA_INTEGER, -ncases);
  vec_labels.attr("class") = classes_not_grouped() ;
  copy_vars(vec_labels, data);

  IntegerVector group_sizes = no_init(ncases);
  for (int i = 0; i < ncases; i++) {
    group_sizes[i] = Rf_length(indices[i]);
  }

  // The attributes are injected into data without duplicating it!
  // The object is mutated, violating R's usual copy-on-write semantics.
  // This is safe here, because the indices are an auxiliary data structure
  // that is rebuilt as necessary. Updating the object in-place saves costly
  // recomputations. We don't touch the "class" attribute here.
  data.attr("indices") = indices;
  data.attr("group_sizes") = group_sizes;
  data.attr("labels") = vec_labels;
}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
void strip_index(DataFrame x) {
  x.attr("indices") = R_NilValue;
  x.attr("group_sizes") = R_NilValue;
  x.attr("labels") = R_NilValue;
}

SEXP strip_group_attributes(SEXP df) {
  Shield<SEXP> attribs(Rf_cons(dplyr::classes_not_grouped(), R_NilValue));
  SET_TAG(attribs, Rf_install("class"));

  SEXP p = ATTRIB(df);
  std::vector<SEXP> black_list(6);
  black_list[0] = Rf_install("indices");
  black_list[1] = Rf_install("vars");
  black_list[2] = Rf_install("index");
  black_list[3] = Rf_install("labels");
  black_list[4] = Rf_install("group_sizes");
  black_list[5] = Rf_install("class");

  SEXP q = attribs;
  while (! Rf_isNull(p)) {
    SEXP tag = TAG(p);
    if (std::find(black_list.begin(), black_list.end(), tag) == black_list.end()) {
      Shield<SEXP> s(Rf_cons(CAR(p), R_NilValue));
      SETCDR(q, s);
      q = CDR(q);
      SET_TAG(q, tag);
    }

    p = CDR(p);
  }
  return attribs;
}
