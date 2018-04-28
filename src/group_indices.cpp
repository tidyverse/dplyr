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

using namespace Rcpp;
using namespace dplyr;

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
  return values ;
}

DataFrame expand_labels(DataFrame labels, bool drop = false) {
  int nc = labels.ncol();
  List uniques(nc);
  std::vector<int> sizes(nc);
  int total_size = 1;

  // doing this with R callbacks for now, might revisit later
  // if this becomes a performance problem
  for (int i = 0; i < nc; i++) {
    SEXP obj = labels[i];
    if (Rf_inherits(obj, "factor")) {
      if (drop) {
        RObject dropped = Language("droplevels", obj).eval() ;
        uniques[i] = unique_levels(dropped) ;
      } else {
        uniques[i] = unique_levels(obj) ;
      }
    } else {
      uniques[i] = Rcpp_eval(Language("unique", obj));
    }
    sizes[i] = Rf_length(uniques[i]);
    total_size *= sizes[i];
  }
  uniques.names() = labels.names();
  uniques.push_back(false, "stringsAsFactors");
  Language call("do.call", Symbol("expand.grid"), uniques);

  DataFrame new_labels = Rcpp_eval(call);

  // cleanup after expand.grid
  new_labels.attr("out.attrs") = R_NilValue;

  IntegerVector new_labels_order = OrderVisitors(new_labels).apply();
  return DataFrameSubsetVisitors(new_labels).subset(new_labels_order, "data.frame");

}

#include <boost/shared_ptr.hpp>

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
    size += other.size ;
  }

  int start ;
  int size ;
};

class ListCollecter {
public:
  ListCollecter(List& data_): data(data_), index(0) {}

  int collect(const std::vector<int>& indices) {
    data[index] = indices ;
    return index++ ;
  }

private:
  List& data ;
  int index ;
};


class CopyVectorVisitor {
public:
  virtual ~CopyVectorVisitor() {} ;

  virtual void copy(const IntRange& target_range, int idx_origin) = 0 ;
};

template <int RTYPE>
class CopyVectorVisitorImpl : public CopyVectorVisitor {
public:
  typedef typename Rcpp::Vector<RTYPE> Vec ;

  CopyVectorVisitorImpl(Vec target_, Vec origin_) :
    target(target_), origin(origin_)
  {}

  virtual void copy(const IntRange& target_range, int idx_origin) {
    std::fill_n(
      target.begin() + target_range.start, target_range.size,
      idx_origin == NA_INTEGER ? Vec::get_na() : origin[idx_origin]
    ) ;
  }

private:
  Vec target;
  Vec origin;
};

inline CopyVectorVisitor* copy_visitor(SEXP target, SEXP origin) {
  switch (TYPEOF(target)) {
  case CPLXSXP:
    return new CopyVectorVisitorImpl<CPLXSXP>(target, origin);
  case INTSXP:
    return new CopyVectorVisitorImpl<INTSXP>(target, origin);
  case REALSXP:
    return new CopyVectorVisitorImpl<REALSXP>(target, origin);
  case LGLSXP:
    return new CopyVectorVisitorImpl<LGLSXP>(target, origin);
  case STRSXP:
    return new CopyVectorVisitorImpl<STRSXP>(target, origin);
  }

  return 0;
}


class Slicer {
public:
  virtual ~Slicer() {} ;
  virtual int size() = 0 ;
  virtual IntRange make(List& vec_labels, const std::vector< boost::shared_ptr<CopyVectorVisitor> >& copy_visitors, ListCollecter& indices_collecter) = 0 ;
} ;
boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_) ;

class LeafSlicer : public Slicer {
public:
  LeafSlicer(const std::vector<int>& index_range_) : index_range(index_range_) {}

  virtual int size() {
    return 1;
  }

  virtual IntRange make(List& vec_labels, const std::vector< boost::shared_ptr<CopyVectorVisitor> >& copy_visitors, ListCollecter& indices_collecter) {
    return IntRange(indices_collecter.collect(index_range), 1) ;
  }

  virtual ~LeafSlicer() {};

private:
  const std::vector<int>& index_range ;
};

class FactorSlicer : public Slicer {
public:
  typedef IntegerVector Factor ;

  FactorSlicer(int depth_, const std::vector<int>& index_range_, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_) :
    depth(depth_),
    index_range(index_range_),
    data(data_),
    visitors(visitors_),

    f(data[depth]),
    nlevels(Rf_length(f.attr("levels"))),

    indices(nlevels),
    slicers(nlevels),
    slicer_size(0)
  {

    // ---- train the slicer, record the indices for each level
    int n = index_range.size();
    for (int i = 0; i < n; i++) {
      int idx = index_range[i];
      int value = f[idx];

      // will support it later
      if (value == NA_INTEGER) stop("NA not supported");
      indices[value - 1].push_back(idx);
    }

    // ---- for each level, train child slicers
    for (int i = 0; i < nlevels; i++) {
      slicers[i] = slicer(indices[i], depth + 1, data, visitors);
      slicer_size += slicers[i]->size() ;
    }

  }

  virtual int size() {
    return slicer_size;
  }

  virtual IntRange make(List& vec_labels, const std::vector< boost::shared_ptr<CopyVectorVisitor> >& copy_visitors, ListCollecter& indices_collecter) {
    IntRange labels_range ;
    SEXP x = vec_labels[depth] ;

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_labels, copy_visitors, indices_collecter) ;
      labels_range.add(idx) ;

      // fill the labels at these indices
      std::fill_n(INTEGER(x) + idx.start, idx.size, i + 1);
    }

    return labels_range ;
  }

  virtual ~FactorSlicer() {}

private:
  int depth ;
  const std::vector<int>& index_range ;

  const std::vector<SEXP>& data ;
  const DataFrameVisitors& visitors ;

  Factor f ;
  int nlevels ;

  std::vector< std::vector<int> > indices ;
  std::vector< boost::shared_ptr<Slicer> > slicers ;
  int slicer_size ;
};

typedef std::pair<int, std::vector<int> > IndicesPair ;

class PairCompare {
public:
  PairCompare(VectorVisitor* v_) : v(v_) {} ;

  bool operator()(const IndicesPair& x, const IndicesPair& y) {
    return v->less(x.first, y.first) ;
  }

private:
  VectorVisitor* v ;
};

class VectorSlicer : public Slicer {
public:

  VectorSlicer(int depth_, const std::vector<int>& index_range_, const std::vector<SEXP>& data_, const DataFrameVisitors& visitors_) :
    depth(depth_),
    index_range(index_range_),
    data(data_),
    visitors(visitors_),

    visitor(visitors_.get(depth)),
    indices(),
    slicer_size(0)
  {

    int n = index_range.size();
    if (n == 0) {
      // deal with special case when index_range is empty

      agents.push_back(NA_INTEGER) ;         // NA is used as a placeholder
      indices.push_back(std::vector<int>()); // empty indices

      slicers.push_back(slicer(indices[0], depth + 1, data, visitors)) ;
      slicer_size = slicers[0]->size() ;

    } else {
      Map map(visitor, n) ;
      // train the map
      for (int i = 0; i < n; i++) {
        int idx = index_range[i];
        map[idx].push_back(idx);
      }

      // fill agents and indices
      int nlevels = map.size();

      std::vector<IndicesPair> map_collect ;
      for (Map::const_iterator it = map.begin(); it != map.end(); ++it) {
        map_collect.push_back(std::make_pair<int, std::vector<int> >(it->first, it->second)) ;
      }
      std::sort(map_collect.begin(), map_collect.end(), PairCompare(visitors.get(depth))) ;

      // make sure the vectors are not resized
      indices.reserve(nlevels);
      agents.reserve(nlevels);
      slicers.reserve(nlevels);

      // ---- for each case, train child slicers
      for (int i = 0; i < nlevels; i++) {
        agents.push_back(map_collect[i].first) ;
        indices.push_back(map_collect[i].second);
        slicers.push_back(slicer(indices[i], depth + 1, data, visitors)) ;
        slicer_size += slicers[i]->size() ;
      }

    }


  }

  virtual int size() {
    return slicer_size ;
  }

  virtual IntRange make(List& vec_labels, const std::vector< boost::shared_ptr<CopyVectorVisitor> >& copy_visitors, ListCollecter& indices_collecter) {
    IntRange labels_range ;
    int nlevels = slicers.size();

    for (int i = 0; i < nlevels; i++) {
      // collect the indices for that level
      IntRange idx = slicers[i]->make(vec_labels, copy_visitors, indices_collecter) ;
      labels_range.add(idx) ;

      // fill the labels at these indices
      copy_visitors[depth]->copy(idx, agents[i]) ;
    }

    return labels_range ;
  }

  virtual ~VectorSlicer() {}

private:
  typedef VisitorSetIndexMap<VectorVisitor, std::vector<int> > Map ;

  int depth ;
  const std::vector<int>& index_range ;

  const std::vector<SEXP> data ;
  const DataFrameVisitors& visitors ;

  VectorVisitor* visitor ;

  std::vector< int > agents ;
  std::vector< std::vector<int> > indices ;
  std::vector< boost::shared_ptr<Slicer> > slicers ;
  int slicer_size ;

};


boost::shared_ptr<Slicer> slicer(const std::vector<int>& index_range, int depth, const std::vector<SEXP>& data, const DataFrameVisitors& visitors) {
  if (depth == data.size()) {
    return boost::shared_ptr<Slicer>(new LeafSlicer(index_range));
  } else {
    SEXP x = data[depth] ;
    if (Rf_isFactor(x)) {
      return boost::shared_ptr<Slicer>(new FactorSlicer(depth, index_range, data, visitors)) ;
    } else {
      return boost::shared_ptr<Slicer>(new VectorSlicer(depth, index_range, data, visitors)) ;
    }
  }
}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
void build_index_cpp(DataFrame& data, bool drop) {
  SymbolVector vars(get_vars(data));
  const int nvars = vars.size();

  CharacterVector names = data.names();
  IntegerVector indx = vars.match_in_table(names);
  std::vector<SEXP> visited_data(nvars);
  CharacterVector label_names(nvars) ;

  for (int i = 0; i < nvars; ++i) {
    int pos = indx[i];
    if (pos == NA_INTEGER) {
      bad_col(vars[i], "is unknown");
    }

    SEXP v = data[pos - 1];
    visited_data[i] = v ;
    label_names[i] = names[pos - 1] ;

    if (!white_list(v) || TYPEOF(v) == VECSXP) {
      bad_col(vars[i], "can't be used as a grouping variable because it's a {type}",
              _["type"] = get_single_class(v));
    }
  }

  DataFrameVisitors visitors(data, vars);

  int n = data.rows();
  std::vector<int> idx(n) ;
  std::iota(idx.begin(), idx.end(), 0);
  boost::shared_ptr<Slicer> s = slicer(idx, 0, visited_data, visitors) ;

  int ncases = s->size();

  // construct the labels data
  List vec_labels(nvars) ;
  List indices(ncases);
  ListCollecter indices_collecter(indices) ;
  std::vector< boost::shared_ptr<CopyVectorVisitor> > copy_visitors;

  for (int i = 0; i < nvars; i++) {
    vec_labels[i] = Rf_allocVector(TYPEOF(visited_data[i]), ncases) ;
    copy_most_attributes(vec_labels[i], visited_data[i]);

    copy_visitors.push_back(
      boost::shared_ptr<CopyVectorVisitor>(copy_visitor(vec_labels[i], visited_data[i]))
    ) ;
  }

  s->make(vec_labels, copy_visitors, indices_collecter) ;

  vec_labels.attr("names") = label_names ;
  vec_labels.attr("row.names") = IntegerVector::create(NA_INTEGER, -ncases) ;
  vec_labels.attr("class") = "data.frame" ;


  IntegerVector group_sizes = no_init(ncases);
  int biggest_group = 0 ;
  for (int i = 0; i < ncases; i++) {
    group_sizes[i] = Rf_length(indices[i]) ;
    biggest_group = std::max(biggest_group, group_sizes[i]);
  }

  // The attributes are injected into data without duplicating it!
  // The object is mutated, violating R's usual copy-on-write semantics.
  // This is safe here, because the indices are an auxiliary data structure
  // that is rebuilt as necessary. Updating the object in-place saves costly
  // recomputations. We don't touch the "class" attribute here.
  data.attr("indices") = indices;
  data.attr("group_sizes") = group_sizes;
  data.attr("biggest_group_size") = biggest_group;
  data.attr("labels") = vec_labels;

}

// Updates attributes in data by reference!
// All these attributes are private to dplyr.
void strip_index(DataFrame x) {
  x.attr("indices") = R_NilValue;
  x.attr("group_sizes") = R_NilValue;
  x.attr("biggest_group_size") = R_NilValue;
  x.attr("labels") = R_NilValue;
}

SEXP strip_group_attributes(SEXP df) {
  Shield<SEXP> attribs(Rf_cons(dplyr::classes_not_grouped(), R_NilValue));
  SET_TAG(attribs, Rf_install("class"));

  SEXP p = ATTRIB(df);
  std::vector<SEXP> black_list(9);
  black_list[0] = Rf_install("indices");
  black_list[1] = Rf_install("vars");
  black_list[2] = Rf_install("index");
  black_list[3] = Rf_install("labels");
  black_list[4] = Rf_install("drop");
  black_list[5] = Rf_install("group_sizes");
  black_list[6] = Rf_install("biggest_group_size");
  black_list[7] = Rf_install("class");
  black_list[8] = Rf_install("expand");

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
