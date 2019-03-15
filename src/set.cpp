#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/match.h>
#include <tools/collapse.h>
#include <tools/BoolResult.h>

#include <dplyr/visitor_set/VisitorSetIndexSet.h>
#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/visitors/join/Column.h>
#include <dplyr/visitors/join/JoinVisitor.h>
#include <dplyr/visitors/join/JoinVisitorImpl.h>

#include <dplyr/visitors/join/DataFrameJoinVisitors.h>

#include <tools/train.h>
#include <dplyr/data/GroupedDataFrame.h>

using namespace Rcpp;
using namespace dplyr;

class RowTrack {
public:
  RowTrack(const std::string& msg, int max_count_ = 10) : ss(), count(0), max_count(max_count_) {
    ss << msg;
  }

  void record(int i) {
    if (count > max_count) return;
    if (count) ss << ", ";
    int idx = i >= 0 ? (i + 1) : -i;
    ss << idx;
    if (count == max_count) ss << "[...]";
    count++;
  }

  bool empty() const {
    return count == 0;
  }

  std::string str() const {
    return ss.str();
  }

private:
  std::stringstream ss;
  int count;
  int max_count;
};

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame_nonames(DataFrame x, DataFrame y, bool convert) {
  int n = x.size();
  if (n != y.size())
    return no_because(tfm::format("different number of columns : %d x %d", n, y.size()));

  if (convert) {
    for (int i = 0; i < n; i++) {
      try {
        boost::scoped_ptr<JoinVisitor> v(
          join_visitor(
            Column(x[i], SymbolString("x")), Column(y[i], SymbolString("y")), true, true
          )
        );
      } catch (...) {
        return no_because("incompatible");
      }
    }
  } else {
    for (int i = 0; i < n; i++) {
      SEXP xi = x[i], yi = y[i];
      if (TYPEOF(xi) != TYPEOF(yi))
        return no_because("incompatible types");

      if (TYPEOF(xi) == INTSXP) {
        if (Rf_inherits(xi, "factor") && Rf_inherits(yi, "factor")) {
          if (same_levels(xi, yi)) continue;
          return no_because("factors with different levels");
        }

        if (Rf_inherits(xi, "factor")) return no_because("cannot compare factor and integer");
        if (Rf_inherits(yi, "factor")) return no_because("cannot compare factor and integer");

      }
    }
  }

  return yes();

}

bool same_factor_levels(SEXP x, SEXP y, std::stringstream& ss, const SymbolString& name) {
  bool res = same_levels(x, y);
  if (!res) {
    ss << "Factor levels not equal for column `" << name.get_utf8_cstring() << "`";
  }
  return res;
}

bool type_compatible(SEXP x, SEXP y) {
  // if one is a matrix but not the other, the types are not compatible
  if (Rf_isMatrix(x) + Rf_isMatrix(y) == 1) {
    return false;
  }

  if (Rf_inherits(x, "Date")) return Rf_inherits(y, "Date");

  switch (TYPEOF(x)) {
  case RAWSXP:
    return TYPEOF(y) == RAWSXP;
  case LGLSXP:
    return TYPEOF(y) == LGLSXP;
  case CPLXSXP:
    return TYPEOF(y) == CPLXSXP;
  case INTSXP:
    if (Rf_isFactor(x)) {
      return TYPEOF(y) == STRSXP || Rf_isFactor(y);
    } else if (Rf_inherits(x, "Date")) {
      return Rf_inherits(y, "Date");
    } else {
      return !Rf_isFactor(y) && (TYPEOF(y) == INTSXP || TYPEOF(y) == REALSXP);
    }
  case REALSXP:
    return TYPEOF(y) == INTSXP || TYPEOF(y) == REALSXP;
  case STRSXP:
    return TYPEOF(y) == STRSXP || Rf_isFactor(y);
  case VECSXP:
    if (Rf_inherits(x, "data.frame")) {
      // TODO: also recurse into the df to check if
      // - same names
      // - same type for each column
      return Rf_inherits(y, "data.frame");
    } else {
      return !Rf_inherits(y, "data.frame");
    }
  default:
    break;
  }
  return false;
}

bool type_same(SEXP x, SEXP y, std::stringstream& ss, const SymbolString& name) {
  // if one is a matrix but not the other, the types are not compatible
  if (Rf_isMatrix(x) + Rf_isMatrix(y) == 1) {
    return false;
  }
  if (Rf_inherits(x, "Date")) return Rf_inherits(y, "Date");

  switch (TYPEOF(x)) {
  case RAWSXP:
    return TYPEOF(y) == RAWSXP;
  case LGLSXP:
    return TYPEOF(y) == LGLSXP;
  case CPLXSXP:
    return TYPEOF(y) == CPLXSXP;
  case INTSXP:
    if (Rf_isFactor(x)) {
      return Rf_isFactor(y) && same_factor_levels(x, y, ss, name);
    } else {
      return !Rf_isFactor(y) && TYPEOF(y) == INTSXP;
    }
  case REALSXP:
    if (Rf_inherits(x, "Date")) {
      return Rf_inherits(y, "Date");
    } else {
      return TYPEOF(y) == REALSXP;
    }
  case STRSXP:
    return TYPEOF(y) == STRSXP;
  case VECSXP:
    if (Rf_inherits(x, "data.frame")) {
      // TODO: also recurse into the df to check if
      // - same names
      // - same type for each column
      return Rf_inherits(y, "data.frame");
    } else {
      return !Rf_inherits(y, "data.frame");
    }
  default:
    break;
  }
  return false;
}

std::string type_describe(SEXP x) {
  if (Rf_isMatrix(x)) {
    return "matrix";
  } else if (Rf_inherits(x, "data.frame")) {
    return get_single_class(x);
  } else if (Rf_inherits(x, "Date")) {
    return "Date";
  } else if (Rf_isFactor(x)) {
    return get_single_class(x);
  } else {
    return get_single_class(x);
  }
}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool convert = false) {
  int n = x.size();

  Rcpp::Shield<SEXP> x_names(Rf_getAttrib(x, symbols::names));
  Rcpp::Shield<SEXP> y_names(Rf_getAttrib(y, symbols::names));

  bool null_x = Rf_isNull(x_names);
  bool null_y = Rf_isNull(y_names);
  if (null_x && !null_y) {
    return no_because("x does not have names, but y does");
  } else if (null_y && !null_x) {
    return no_because("y does not have names, but x does");
  } else if (null_x && null_y) {
    return compatible_data_frame_nonames(x, y, convert);
  }

  CharacterVector names_x(x_names);
  CharacterVector names_y(y_names);

  CharacterVector names_y_not_in_x = setdiff(names_y, names_x);
  CharacterVector names_x_not_in_y = setdiff(names_x, names_y);

  if (!ignore_col_order) {
    if (names_y_not_in_x.size() == 0 && names_x_not_in_y.size() == 0) {
      // so the names are the same, check if they are in the same order
      for (int i = 0; i < n; i++) {
        if (names_x[i] != names_y[i]) {
          return no_because("Same column names, but different order");
        }
      }
    }
  }

  CharacterVector why;
  if (names_y_not_in_x.size()) {
    std::stringstream ss;
    ss << "Cols in y but not x: " << collapse_utf8(names_y_not_in_x, ", ", "`") << ". ";
    why.push_back(String(ss.str(), CE_UTF8));
  }

  if (names_x_not_in_y.size()) {
    std::stringstream ss;
    ss << "Cols in x but not y: " << collapse_utf8(names_x_not_in_y, ", ", "`") << ". ";
    why.push_back(String(ss.str(), CE_UTF8));
  }

  if (why.length() > 0) return no_because(why);

  IntegerVector orders(r_match(names_x, names_y));

  for (int i = 0; i < n; i++) {
    SymbolString name = names_x[i];
    SEXP xi = x[i], yi = y[orders[i] - 1];

    std::stringstream ss;
    bool compatible = convert ? type_compatible(xi, yi) : type_same(xi, yi, ss, name);

    if (!compatible) {
      if (ss.str() == "") {
        ss << "Incompatible type for column `"
           << name.get_utf8_cstring()
           << "`: x " << type_describe(xi)
           << ", y " << type_describe(yi);
      }

      why.push_back(String(ss.str(), CE_UTF8));
    }

  }

  if (why.length() > 0) return no_because(why);
  return yes();
}

// [[Rcpp::export]]
dplyr::BoolResult equal_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false) {
  BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
  if (!compat) return compat;

  typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map;
  SymbolVector x_names(Rf_getAttrib(x, symbols::names));
  DataFrameJoinVisitors visitors(x, y, x_names, x_names, true, true);
  Map map(visitors);

  // train the map in both x and y
  int nrows_x = x.nrows();
  int nrows_y = y.nrows();

  if (nrows_x != nrows_y)
    return no_because("Different number of rows");
  if (x.size() == 0)
    return yes();

  for (int i = 0; i < nrows_x; i++) map[i].push_back(i);
  for (int i = 0; i < nrows_y; i++) map[-i - 1].push_back(-i - 1);

  RowTrack track_x("Rows in x but not y: ");
  RowTrack track_y("Rows in y but not x: ");
  RowTrack track_mismatch("Rows with difference occurences in x and y: ");

  bool ok = true;
  Map::const_iterator it = map.begin();

  for (; it != map.end(); ++it) {
    // retrieve the indices ( -ves for y, +ves for x )
    const std::vector<int>& chunk = it->second;
    int n = chunk.size();

    int count_left = 0, count_right = 0;
    for (int i = 0; i < n; i++) {
      if (chunk[i] < 0)
        count_right++;
      else
        count_left++;
    }
    if (count_right == 0) {
      track_x.record(chunk[0]);
      ok = false;
    } else if (count_left == 0) {
      track_y.record(chunk[0]);
      ok = false;
    } else if (count_left != count_right) {
      track_mismatch.record(chunk[0]);
      ok = false;
    }

  }

  if (!ok) {
    std::stringstream ss;
    if (! track_x.empty()) ss << track_x.str() << ". ";
    if (! track_y.empty()) ss << track_y.str() << ". ";
    if (! track_mismatch.empty()) ss << track_mismatch.str();

    return no_because(CharacterVector::create(String(ss.str(), CE_UTF8)));
  }

  if (ok && ignore_row_order) return yes();

  if (!ignore_row_order) {
    for (int i = 0; i < nrows_x; i++) {
      if (!visitors.equal(i, -i - 1)) {
        return no_because("Same row values, but different order");
      }
    }
  }

  return yes();
}

DataFrame reconstruct_metadata(DataFrame out, const DataFrame& x) {
  if (is<GroupedDataFrame>(x)) {
    // go through the GroupedDataFrame class so that the groups attribute is generated
    return GroupedDataFrame(out, x).data();
  } else {
    // nothing to do for rowwise and natural data frames
    return out;
  }
}

// [[Rcpp::export]]
DataFrame union_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  SymbolVector x_names(Rf_getAttrib(x, symbols::names));
  DataFrameJoinVisitors visitors(x, y, x_names, x_names, true, true);
  Set set(visitors);

  int n_x = x.nrows();
  int n_y = y.nrows();

  std::vector<int> indices;
  indices.reserve(n_x + n_y);

  for (int i = 0; i < n_x; i++) {
    std::pair<Set::iterator, bool> inserted = set.insert(i);
    if (inserted.second) {
      indices.push_back(i);
    }
  }

  for (int i = 0; i < n_y; i++) {
    std::pair<Set::iterator, bool> inserted = set.insert(-i - 1);
    if (inserted.second) {
      indices.push_back(-i - 1);
    }
  }

  return reconstruct_metadata(visitors.subset(indices, get_class(x)), x);
}

// [[Rcpp::export]]
DataFrame intersect_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  SymbolVector x_names(Rf_getAttrib(x, symbols::names));
  DataFrameJoinVisitors visitors(x, y, x_names, x_names, true, true);
  Set set(visitors);

  int n_x = x.nrows();
  int n_y = y.nrows();

  train_insert_right(set, n_y);

  std::vector<int> indices;
  indices.reserve(std::min(n_x, n_y));

  for (int i = 0; i < n_x; i++) {
    Set::iterator it = set.find(i);
    if (it != set.end()) {
      indices.push_back(*it);
      set.erase(it);
    }
  }

  return reconstruct_metadata(visitors.subset(indices, get_class(x)), x);
}

// [[Rcpp::export]]
DataFrame setdiff_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  SymbolVector y_names(Rf_getAttrib(y, symbols::names));
  DataFrameJoinVisitors visitors(x, y, y_names, y_names, true, true);
  Set set(visitors);

  int n_x = x.nrows();
  int n_y = y.nrows();

  train_insert_right(set, n_y);

  std::vector<int> indices;
  indices.reserve(n_x);

  for (int i = 0; i < n_x; i++) {
    std::pair<Set::iterator, bool> inserted = set.insert(i);
    if (inserted.second) {
      indices.push_back(i);
    }
  }

  return reconstruct_metadata(visitors.subset(indices, get_class(x)), x);
}
