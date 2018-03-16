#ifndef dplyr_DataFrameJoinVisitors_H
#define dplyr_DataFrameJoinVisitors_H

#include <tools/pointer_vector.h>

#include <dplyr/visitor_set/VisitorSetMixin.h>

#include <dplyr/tbl_cpp.h>
#include <dplyr/JoinVisitor.h>

namespace dplyr {

class DataFrameJoinVisitors :
  public VisitorSetEqual<DataFrameJoinVisitors>,
  public VisitorSetHash<DataFrameJoinVisitors>
{
public:
  DataFrameJoinVisitors(
    const DataFrame& left_,
    const DataFrame& right_,
    const SymbolVector& names_left,
    const SymbolVector& names_right,
    bool warn_,
    bool na_match
  );

  DataFrameJoinVisitors(
    const DataFrame& left_,
    const DataFrame& right_,
    const IntegerVector& indices_left,
    const IntegerVector& indices_right,
    bool warn_,
    bool na_match
  );

  JoinVisitor* get(int k) const;
  JoinVisitor* get(const SymbolString& name) const;
  int size() const;

  template <typename Container>
  inline DataFrame subset(const Container& index, const CharacterVector& classes) {
    int nrows = index.size();
    const int nvisitors = size();
    Rcpp::List out(nvisitors);
    for (int k = 0; k < nvisitors; k++) {
      out[k] = get(k)->subset(index);
    }
    set_class(out, classes);
    set_rownames(out, nrows);
    out.names() = visitor_names_left;
    copy_vars(out, left);
    return (SEXP)out;
  }

private:
  const DataFrame& left;
  const DataFrame& right;
  SymbolVector visitor_names_left;
  SymbolVector visitor_names_right;

  pointer_vector<JoinVisitor> visitors;
  bool warn;

};

}

#endif
