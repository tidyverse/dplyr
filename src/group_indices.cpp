#include "pch.h"
#include <dplyr/main.h>
#include <dplyr/allow_list.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>
#include <dplyr/visitors/join/DataFrameJoinVisitors.h>
#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/visitors/order/Order.h>
#include <tools/train.h>

#include <tools/bad.h>
#include <tools/match.h>
#include <boost/shared_ptr.hpp>
#include <tools/default_value.h>
#include <tools/utils.h>

#include <dplyr/hybrid/scalar_result/n.h>
#include <dplyr/symbols.h>
#include <dplyr/visitors/subset/column_subset.h>
#include <dplyr/visitors/subset/DataFrameSelect.h>

using namespace Rcpp;
using namespace dplyr;

# if __cplusplus >= 201103L
#define MOVE(x) std::move(x)
# else
#define MOVE(x) x
# endif

// [[Rcpp::export(rng = false)]]
IntegerVector grouped_indices_grouped_df_impl(const GroupedDataFrame& gdf) {
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

// [[Rcpp::export(rng = false)]]
IntegerVector group_size_grouped_cpp(const GroupedDataFrame& gdf) {
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
boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_, bool drop);

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

  FactorSlicer(int depth_, const std::vector<int>& index_range, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_, bool drop_) :
    depth(depth_),
    data(data_),
    visitors(visitors_),

    f(data[depth]),
    nlevels(Rf_length(Rf_getAttrib(f, symbols::levels))),

    indices(nlevels + 1),
    slicers(nlevels + 1),
    slicer_size(0),
    has_implicit_na(false),
    drop(drop_)
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
      slicers[i] = slicer(indices[i], depth + 1, data, visitors, drop);
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
  bool drop;
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

  VectorSlicer(int depth_, const std::vector<int>& index_range, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_, bool drop_) :
    depth(depth_),
    // index_range(index_range_),
    data(data_),
    visitors(visitors_),

    visitor(visitors_.get(depth)),
    indices(),
    slicer_size(0),
    drop(drop_)
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
      slicers.push_back(slicer(indices[i], depth + 1, data, visitors, drop));
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
  bool drop;
};

boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data, const DataFrameVisitors& visitors, bool drop) {
  if (static_cast<size_t>(depth) == data.size()) {
    return boost::shared_ptr<Slicer>(new LeafSlicer(index_range));
  } else {
    SEXP x = data[depth];
    if (Rf_isFactor(x) && !drop) {
      return boost::shared_ptr<Slicer>(new FactorSlicer(depth, index_range, data, visitors, drop));
    } else {
      return boost::shared_ptr<Slicer>(new VectorSlicer(depth, index_range, data, visitors, drop));
    }
  }
}

inline bool is_factor(SEXP x) {
  return Rf_inherits(x, "factor");
}

bool has_no_factors(const std::vector<SEXP>& x) {
  return std::find_if(x.begin(), x.end(), is_factor) == x.end();
}

// [[Rcpp::export(rng = false)]]
SEXP regroup(DataFrame grouping_data, SEXP frame) {
  size_t nc = grouping_data.size() - 1;

  // 1) only keep the rows with non empty groups
  size_t n = grouping_data.nrow();
  std::vector<int> keep;
  keep.reserve(n);
  ListView rows = grouping_data[nc];
  for (size_t i = 0; i < n; i++) {
    if (LENGTH(rows[i]) > 0) keep.push_back(i + 1);
  }
  if (keep.size() == n) return grouping_data;
  Rcpp::IntegerVector r_keep(keep.begin(), keep.end());
  grouping_data = dataframe_subset(grouping_data, r_keep, "data.frame", frame);

  // 2) perform a group by so that factor levels are expanded
  DataFrameVisitors visitors(grouping_data, nc);
  std::vector<SEXP> visited_data(nc);
  for (size_t i = 0; i < nc; i++) {
    visited_data[i] = grouping_data[i];
  }
  SEXP drop = Rf_getAttrib(grouping_data, symbols::dot_drop);
  boost::shared_ptr<Slicer> s = slicer(std::vector<int>(), 0, visited_data, visitors, is<bool>(drop) && as<bool>(drop));
  size_t ncases = s->size();
  if (ncases == 1 && grouping_data.nrow() == 0 && has_no_factors(visited_data)) {
    ncases = 0;
  }


  List vec_groups(nc + 1);
  List indices(ncases);
  ListCollecter indices_collecter(indices);

  for (size_t i = 0; i < nc; i++) {
    vec_groups[i] = Rf_allocVector(TYPEOF(visited_data[i]), ncases);
    copy_most_attributes(vec_groups[i], visited_data[i]);
  }

  if (ncases > 0) {
    s->make(vec_groups, indices_collecter);
  }

  // 3) translate indices on grouping_data to indices wrt the data
  ListView original_rows = grouping_data[nc];
  for (size_t i = 0; i < ncases; i++) {
    if (LENGTH(indices[i]) == 1) {
      indices[i] = original_rows[as<int>(indices[i]) - 1];
    }
  }
  vec_groups[nc] = indices;

  Rf_namesgets(vec_groups, vec_names(grouping_data));
  set_rownames(vec_groups, ncases);
  Rf_classgets(vec_groups, NaturalDataFrame::classes());

  return vec_groups;
}


SEXP build_index_cpp(const DataFrame& data, const SymbolVector& vars, bool drop) {
  const int nvars = vars.size();

  Shield<SEXP> names(Rf_getAttrib(data, symbols::names));
  Shield<SEXP> indx(r_match(vars.get_vector(), names));
  int* p_indx = INTEGER(indx);
  std::vector<SEXP> visited_data(nvars);
  CharacterVector groups_names(nvars + 1);

  for (int i = 0; i < nvars; ++i) {
    int pos = p_indx[i];
    if (pos == NA_INTEGER) {
      bad_col(vars[i], "is unknown");
    }

    SEXP v = data[pos - 1];
    visited_data[i] = v;
    groups_names[i] = STRING_ELT(names, pos - 1);

    if (!allow_list(v) || TYPEOF(v) == VECSXP) {
      bad_col(vars[i], "can't be used as a grouping variable because it's a {type}",
              _["type"] = get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);

  boost::shared_ptr<Slicer> s = slicer(std::vector<int>(), 0, visited_data, visitors, drop);
  int ncases = s->size();
  if (ncases == 1 && data.nrow() == 0 && has_no_factors(visited_data)) {
    ncases = 0;
  }

  // construct the groups data
  List vec_groups(nvars + 1);
  List indices(ncases);

  for (int i = 0; i < nvars; i++) {
    vec_groups[i] = Rf_allocVector(TYPEOF(visited_data[i]), ncases);
    copy_most_attributes(vec_groups[i], visited_data[i]);
  }
  ListCollecter indices_collecter(indices);
  if (ncases > 0) {
    s->make(vec_groups, indices_collecter);
  }

  vec_groups[nvars] = indices;
  groups_names[nvars] = ".rows";

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

  Rf_namesgets(vec_groups, groups_names);
  set_rownames(vec_groups, ncases);
  Rf_classgets(vec_groups, NaturalDataFrame::classes());
  Rf_setAttrib(vec_groups, symbols::dot_drop, Rf_ScalarLogical(drop));

  return vec_groups;
}

namespace dplyr {

SEXP check_grouped(RObject data) {
  // compat with old style grouped data frames
  SEXP vars = Rf_getAttrib(data, symbols::vars);

  if (!Rf_isNull(vars)) {
    Rf_warningcall(R_NilValue, "Detecting old grouped_df format, replacing `vars` attribute by `groups`");

    // only make the groups attribute if it does not yet exist
    if (Rf_isNull(Rf_getAttrib(data, symbols::groups))) {
      // using drop = true here because this is likely to play better with
      // older representations
      DataFrame groups = build_index_cpp(data, SymbolVector(vars), true);
      Rf_setAttrib(data, symbols::groups, groups);
    }

    // but always clean the pre 0.8.0 attributes
    Rf_setAttrib(data, symbols::vars, R_NilValue);
    Rf_setAttrib(data, symbols::indices, R_NilValue);
    Rf_setAttrib(data, symbols::labels, R_NilValue);

  }

  // get the groups attribute and check for consistency
  SEXP groups = Rf_getAttrib(data, symbols::groups);

  // groups must be a data frame
  if (!is<DataFrame>(groups)) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must be a data frame");
  }

  int nc = Rf_length(groups);

  // it must have at least 1 column
  if (nc < 1) {
    bad_arg(".data", "is a corrupt grouped_df, the `\"groups\"` attribute must have at least one column");
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
  symbols(group_vars()),
  groups(Rf_getAttrib(data_, symbols::groups)),
  nvars_(symbols.size())
{}

GroupedDataFrame::GroupedDataFrame(DataFrame x, const GroupedDataFrame& model):
  data_(x),
  symbols(model.get_vars()),
  groups(build_index_cpp(data_, model.get_vars(), model.drops())),
  nvars_(symbols.size())
{
  set_groups(data_, groups);
}

SymbolVector GroupedDataFrame::group_vars() const {
  SEXP groups = Rf_getAttrib(data_, dplyr::symbols::groups);

  int n = Rf_length(groups) - 1;
  Rcpp::Shelter<SEXP> shelter;
  SEXP vars_attr = shelter(Rf_getAttrib(groups, R_NamesSymbol));
  SEXP vars = shelter(Rf_allocVector(STRSXP, n));
  for (int i = 0; i < n; i++) {
    SET_STRING_ELT(vars, i, STRING_ELT(vars_attr, i));
  }
  return SymbolVector(vars);
}

}

// [[Rcpp::export(rng = false)]]
DataFrame grouped_df_impl(DataFrame data, SymbolVector symbols, bool drop) {
  DataFrame copy(shallow_copy(data));

  if (!symbols.size()) {
    GroupedDataFrame::strip_groups(copy);
    Rf_classgets(copy, NaturalDataFrame::classes());
    return copy;
  }

  set_class(copy, GroupedDataFrame::classes());

  // we've made a copy and we are about to create the groups
  // attribute, so we make sure there is no more a vars
  // attribute lurking around from the pre 0.8.0 area
  Rf_setAttrib(copy, symbols::vars, R_NilValue);
  Rf_setAttrib(copy, symbols::drop, R_NilValue);

  GroupedDataFrame::set_groups(copy, build_index_cpp(copy, symbols, drop));

  return copy;
}

// [[Rcpp::export(rng = false)]]
DataFrame group_data_grouped_df(DataFrame data) {
  return GroupedDataFrame(data).group_data();
}

// [[Rcpp::export(rng = false)]]
DataFrame ungroup_grouped_df(DataFrame df) {
  DataFrame copy(shallow_copy(df));
  GroupedDataFrame::strip_groups(copy);
  set_class(copy, NaturalDataFrame::classes());
  return copy;
}

// [[Rcpp::export(rng = false)]]
List group_split_impl(const GroupedDataFrame& gdf, bool keep, SEXP frame, bool ptype) {
  ListView rows = gdf.indices();
  R_xlen_t n = rows.size();

  DataFrame group_data = gdf.group_data();
  DataFrame data = gdf.data();

  if (!keep) {
    Shield<SEXP> all_names(vec_names(data));
    int nv = data.size();
    dplyr_hash_set<SEXP> all_set;
    for (int i = 0; i < nv; i++) {
      all_set.insert(STRING_ELT(all_names, i));
    }

    int ng = group_data.ncol() - 1;
    Rcpp::Shield<SEXP> group_names(vec_names(group_data));
    for (int i = 0; i < ng; i++) {
      SEXP name = STRING_ELT(group_names, i);
      if (all_set.count(name)) all_set.erase(name);
    }

    IntegerVector kept_cols(all_set.size());
    int k = 0;
    for (int i = 0; i < nv; i++) {
      if (all_set.count(STRING_ELT(all_names, i))) {
        kept_cols[k++] = i + 1;
      }
    }
    data = DataFrameSelect(data, kept_cols, false);
  }

  GroupedDataFrame::group_iterator git = gdf.group_begin();
  List out(n);
  for (R_xlen_t i = 0; i < n; i++, ++git) {
    DataFrame out_i = dataframe_subset(data, *git, NaturalDataFrame::classes(), frame);
    GroupedDataFrame::strip_groups(out_i);
    out[i] = out_i;
  }
  if (ptype) {
    Rf_setAttrib(
      out, symbols::ptype,
      dataframe_subset(data, IntegerVector(0), NaturalDataFrame::classes(), frame)
    );
  }
  return out;
}
