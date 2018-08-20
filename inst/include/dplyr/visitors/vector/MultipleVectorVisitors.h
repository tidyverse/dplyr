#ifndef dplyr_MultipleVectorVisitors_H
#define dplyr_MultipleVectorVisitors_H

#include <boost/shared_ptr.hpp>

#include <dplyr/visitor_set/VisitorSetEqual.h>
#include <dplyr/visitor_set/VisitorSetHash.h>
#include <dplyr/visitor_set/VisitorSetLess.h>
#include <dplyr/visitor_set/VisitorSetGreater.h>

#include <dplyr/visitors/vector/visitor.h>
#include <tools/utils.h>

namespace dplyr {

class MultipleVectorVisitors :
  public VisitorSetEqual<MultipleVectorVisitors>,
  public VisitorSetHash<MultipleVectorVisitors>,
  public VisitorSetLess<MultipleVectorVisitors>,
  public VisitorSetGreater<MultipleVectorVisitors> {

private:
  std::vector< boost::shared_ptr<VectorVisitor> > visitors;
  int length;
  int ngroups;

public:
  typedef VectorVisitor visitor_type;

  MultipleVectorVisitors(List data, int nrows, int ngroups_, int g) :
    visitors(),
    length(nrows),
    ngroups(ngroups_)
  {
    int n = data.size();
    for (int i = 0; i < n; i++) {
      push_back(data[i], g);
    }
  }

  inline int size() const {
    return visitors.size();
  }

  inline VectorVisitor* get(int k) const {
    return visitors[k].get();
  }

  inline int nrows() const {
    if (visitors.size() == 0) {
      stop("Need at least one column for `nrows()`");
    }
    return visitors[0]->size();
  }

  inline bool is_na(int index) const {
    int n = size();
    for (int i = 0; i < n; i++) if (visitors[i]->is_na(index)) return true;
    return false;
  }

private:

  inline void push_back(SEXP x, int g) {
    int s = get_size(x);
    if (s == length) {
      visitors.push_back(boost::shared_ptr<VectorVisitor>(visitor(x)));
    } else if (s == ngroups) {
      visitors.push_back(boost::shared_ptr<VectorVisitor>(recycling_visitor(x, g, length)));
    } else {
      stop("incompatible size, should be either %d or %d (thr number of groups)", length, ngroups);
    }
  }

};

} // namespace dplyr

#include <dplyr/visitors/vector/visitor_impl.h>

#endif
