#ifndef dplyr_Gatherer_H
#define dplyr_Gatherer_H

#include <tools/all_na.h>
#include <tools/hash.h>
#include <tools/utils.h>

#include <dplyr/checks.h>

#include <tools/vector_class.h>
#include <dplyr/checks.h>
#include <dplyr/Collecter.h>
#include <dplyr/bad.h>

namespace dplyr {

template <typename Data>
inline const char* check_length_message() {
  return "the group size";
}
template <>
inline const char* check_length_message<NaturalDataFrame>() {
  return "the number of rows";
}

template <typename Data, typename Subsets, typename Proxy>
class Gatherer {
public:
  typedef typename Data::slicing_index Index;

  Gatherer(const RObject& first, const Index& indices, Proxy& proxy_, const Data& gdf_, int first_non_na_, const SymbolString& name_) :
    gdf(gdf_), proxy(proxy_), first_non_na(first_non_na_), name(name_)
  {
    coll = collecter(first, gdf.nrows());
    if (first_non_na < gdf.ngroups())
      grab(first, indices);
  }

  ~Gatherer() {
    if (coll != 0) {
      delete coll;
    }
  }

  SEXP collect() {
    int ngroups = gdf.ngroups();
    if (first_non_na == ngroups) return coll->get();
    typename Data::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      Shield<SEXP> subset(proxy.get(indices));
      grab(subset, indices);
    }
    return coll->get();
  }

private:

  inline void grab(SEXP subset, const Index& indices) {
    int n = Rf_length(subset);
    if (n == indices.size()) {
      grab_along(subset, indices);
    } else if (n == 1) {
      grab_rep(subset, indices);
    } else if (Rf_isNull(subset)) {
      stop("incompatible types (NULL), expecting %s", coll->describe());
    } else {
      check_length(n, indices.size(), check_length_message<Data>(), name);
    }
  }

  void grab_along(SEXP subset, const SlicingIndex& indices) {
    if (coll->compatible(subset)) {
      // if the current source is compatible, collect
      coll->collect(indices, subset);
    } else if (coll->can_promote(subset)) {
      // setup a new Collecter
      Collecter* new_collecter = promote_collecter(subset, gdf.nrows(), coll);

      // import data from previous collecter.
      new_collecter->collect(NaturalSlicingIndex(gdf.nrows()), coll->get());

      // import data from this chunk
      new_collecter->collect(indices, subset);

      // dispose the previous collecter and keep the new one.
      delete coll;
      coll = new_collecter;
    } else if (coll->is_logical_all_na()) {
      Collecter* new_collecter = collecter(subset, gdf.nrows());
      new_collecter->collect(indices, subset);
      delete coll;
      coll = new_collecter;
    } else {
      bad_col(name, "can't be converted from {source_type} to {target_type}",
              _["source_type"] = coll->describe(), _["target_type"] = get_single_class(subset));
    }
  }

  void grab_rep(SEXP value, const SlicingIndex& indices) {
    int n = indices.size();
    // FIXME: This can be made faster if `source` in `Collecter->collect(source, indices)`
    //        could be of length 1 recycling the value.
    // TODO: create Collecter->collect_one(source, indices)?
    for (int j = 0; j < n; j++) {
      grab_along(value, RowwiseSlicingIndex(indices[j]));
    }
  }

  const Data& gdf;
  Proxy& proxy;
  Collecter* coll;
  int first_non_na;
  const SymbolString& name;

};

template <typename Data, typename Subsets, typename Proxy>
class ListGatherer {
public:
  typedef typename Data::slicing_index Index;

  ListGatherer(List first, const Index& indices, Proxy& proxy_, const Data& gdf_, int first_non_na_, const SymbolString& name_) :
    gdf(gdf_), proxy(proxy_), data(gdf.nrows()), first_non_na(first_non_na_), name(name_)
  {
    if (first_non_na < gdf.ngroups()) {
      grab(first, indices);
    }

    copy_most_attributes(data, first);
  }

  SEXP collect() {
    int ngroups = gdf.ngroups();
    if (first_non_na == ngroups) return data;
    typename Data::group_iterator git = gdf.group_begin();
    int i = 0;
    for (; i < first_non_na; i++) ++git;
    ++git;
    i++;
    for (; i < ngroups; i++, ++git) {
      const Index& indices = *git;
      List subset(proxy.get(indices));
      grab(subset, indices);
    }
    return data;
  }

private:

  inline void grab(const List& subset, const Index& indices) {
    int n = subset.size();

    if (n == indices.size()) {
      grab_along(subset, indices);
    } else if (n == 1) {
      grab_rep(subset[0], indices);
    } else {
      check_length(n, indices.size(), check_length_message<Data>(), name);
    }
  }

  void grab_along(const List& subset, const Index& indices) {
    int n = indices.size();
    for (int j = 0; j < n; j++) {
      data[ indices[j] ] = subset[j];
    }
  }

  void grab_rep(SEXP value, const Index& indices) {
    int n = indices.size();
    for (int j = 0; j < n; j++) {
      data[ indices[j] ] = value;
    }
  }

  const Data& gdf;
  Proxy& proxy;
  List data;
  int first_non_na;
  const SymbolString name;

};


} // namespace dplyr


#endif

