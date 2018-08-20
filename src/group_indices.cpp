#include "pch.h"
#include <dplyr/main.h>
#include <dplyr/white_list.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/visitors/join/DataFrameJoinVisitors.h>

#include <dplyr/visitors/order/Order.h>
#include <dplyr/train.h>

#include <tools/bad.h>
#include <dplyr/tbl_cpp.h>

#include <tools/match.h>
#include <boost/shared_ptr.hpp>
#include <dplyr/default_value.h>

#include <dplyr/hybrid/scalar_result/n.h>

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
  IntegerVector res(no_init(n));
  int ngroups = gdf.ngroups();
  GroupedDataFrameIndexIterator it = gdf.group_begin();
  for (int i = 0; i < ngroups; i++, ++it) {
    const GroupedSlicingIndex& index = *it;
    int n_index = index.size();
    for (int j = 0; j < n_index; j++) {
      res[ index[j] ] = i + 1;
    }
  }
  return res;
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp(GroupedDataFrame gdf) {
  return hybrid::n_(gdf).summarise() ;
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

inline int plus_one(int i) {
  return i + 1;
}

class ListCollecter {
public:
  ListCollecter(List& data_): data(data_), index(0) {}

  int collect(const std::vector<int>& indices) {
    data[index] = IntegerVector(indices.begin(), indices.end(), plus_one);
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
  virtual IntRange make(List& vec_groups, ListCollecter& indices_collecter) = 0;
};
boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_);

class LeafSlicer : public Slicer {
public:
  LeafSlicer(const std::vector<int>& index_range_) : index_range(index_range_) {}

  virtual int size() {
    return 1;
  }

  virtual IntRange make(List& vec_groups, ListCollecter& indices_collecter) {
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

  virtual IntRange make(List& vec_groups, ListCollecter& indices_collecter) {
    IntRange groups_range;
    SEXP x = vec_groups[depth];

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_groups, indices_collecter);
      groups_range.add(idx);

      // fill the groups at these indices
      std::fill_n(INTEGER(x) + idx.start, idx.size, i + 1);
    }

    if (has_implicit_na) {
      // collect the indices for the implicit NA pseudo group
      IntRange idx = slicers[nlevels]->make(vec_groups, indices_collecter);
      groups_range.add(idx);

      // fill the groups at these indices
      std::fill_n(INTEGER(x) + idx.start, idx.size, NA_INTEGER);
    }

    return groups_range;
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

  virtual IntRange make(List& vec_groups, ListCollecter& indices_collecter) {
    IntRange groups_range;
    int nlevels = slicers.size();

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_groups, indices_collecter);
      groups_range.add(idx);

      // fill the groups at these indices
      copy_visit(idx, agents[i], vec_groups[depth], data[depth]);
    }

    return groups_range;
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
  if (static_cast<size_t>(depth) == data.size()) {
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

SEXP build_index_cpp(const DataFrame& data, const SymbolVector& vars) {
  const int nvars = vars.size();

  CharacterVector names = data.names();
  IntegerVector indx = vars.match_in_table(names);
  std::vector<SEXP> visited_data(nvars);
  CharacterVector groups_names(nvars + 1);

  for (int i = 0; i < nvars; ++i) {
    int pos = indx[i];
    if (pos == NA_INTEGER) {
      bad_col(vars[i], "is unknown");
    }

    SEXP v = data[pos - 1];
    visited_data[i] = v;
    groups_names[i] = names[pos - 1];

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      bad_col(vars[i], "can't be used as a grouping variable because it's a {type}",
              _["type"] = get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);

  boost::shared_ptr<Slicer> s = slicer(std::vector<int>(), 0, visited_data, visitors);

  int ncases = s->size();

  // construct the groups data
  List vec_groups(nvars + 1);
  List indices(ncases);
  ListCollecter indices_collecter(indices);

  for (int i = 0; i < nvars; i++) {
    vec_groups[i] = Rf_allocVector(TYPEOF(visited_data[i]), ncases);
    copy_most_attributes(vec_groups[i], visited_data[i]);
  }

  vec_groups[nvars] = indices;
  groups_names[nvars] = ".rows";
  s->make(vec_groups, indices_collecter);

  // warn about NA in factors
  for (int i = 0; i < nvars; i++) {
    SEXP x = vec_groups[i];
    if (Rf_isFactor(x)) {
      IntegerVector xi(x);
      if (std::find(xi.begin(), xi.end(), NA_INTEGER) < xi.end()) {
        warningcall(R_NilValue, tfm::format("Factor `%s` contains implicit NA, consider using `forcats::fct_explicit_na`", CHAR(groups_names[i].get())));
      }
    }
  }

  vec_groups.attr("names") = groups_names;
  vec_groups.attr("row.names") = IntegerVector::create(NA_INTEGER, -ncases);
  vec_groups.attr("class") = classes_not_grouped() ;

  return vec_groups;
}

namespace dplyr {

SEXP check_grouped(RObject data) {
  static SEXP groups_symbol = Rf_install("groups");
  static SEXP vars_symbol = Rf_install("vars");

  // compat with old style grouped data frames
  SEXP vars = Rf_getAttrib(data, vars_symbol);
  if (!Rf_isNull(vars)) {
    DataFrame groups = build_index_cpp(data, SymbolVector(vars));
    data.attr("groups") = groups;
  }

  // get the groups attribute and check for consistency
  SEXP groups = Rf_getAttrib(data, groups_symbol);

  // groups must be a data frame
  if (!is<DataFrame>(groups)) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must be a data frame");
  }

  // it must have at least 1 column
  int nc = Rf_length(groups);
  if (nc <= 1) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must have at least two columns");
  }

  // the last column must be a list and called `.rows`
  SEXP names = Rf_getAttrib(groups, R_NamesSymbol);
  SEXP last = VECTOR_ELT(groups, nc - 1);
  static String rows(".rows");
  if (TYPEOF(last) != VECSXP || STRING_ELT(names, nc - 1) != rows.get_sexp()) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must have a list column named `.rows` as last column");
  }

  return data ;
}

GroupedDataFrame::GroupedDataFrame(DataFrame x):
  data_(check_grouped(x)),
  symbols(group_vars(data_)),
  groups(data_.attr("groups")),
  nvars_(symbols.size())
{
  int rows_in_groups = 0;
  int ng = ngroups();
  List idx = indices();
  for (int i = 0; i < ng; i++) rows_in_groups += Rf_length(idx[i]);
  if (data_.nrows() != rows_in_groups) {
    bad_arg(".data", "is a corrupt grouped_df, contains {rows} rows, and {group_rows} rows in groups",
            _["rows"] = data_.nrows(), _["group_rows"] = rows_in_groups);
  }
}

GroupedDataFrame::GroupedDataFrame(DataFrame x, const GroupedDataFrame& model):
  data_(x),
  symbols(model.get_vars()),
  groups(build_index_cpp(data_, model.get_vars())),
  nvars_(symbols.size())
{
  set_groups(data_, groups);
}

SymbolVector GroupedDataFrame::group_vars(SEXP x) {
  check_grouped(x);

  static SEXP groups_symbol = Rf_install("groups");
  SEXP groups = Rf_getAttrib(x, groups_symbol);

  int n = Rf_length(groups) - 1;
  CharacterVector vars = Rf_getAttrib(groups, R_NamesSymbol);
  vars.erase(n);
  return SymbolVector(vars);
}

}

// [[Rcpp::export]]
DataFrame grouped_df_impl(DataFrame data, SymbolVector symbols) {
  assert_all_white_list(data);
  DataFrame copy(shallow_copy(data));
  set_class(copy, classes_grouped<GroupedDataFrame>());
  if (!symbols.size())
    stop("no variables to group by");
  GroupedDataFrame::set_groups(copy, build_index_cpp(copy, symbols));
  return copy;
}

// [[Rcpp::export]]
DataFrame group_data_grouped_df(DataFrame data) {
  return GroupedDataFrame(data).group_data();
}
