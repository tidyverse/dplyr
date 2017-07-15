#include "pch.h"
#include <dplyr/main.h>

#include <boost/scoped_ptr.hpp>

#include <tools/match.h>
#include <tools/collapse.h>

#include <dplyr/visitor_set/VisitorSetIndexSet.h>
#include <dplyr/visitor_set/VisitorSetIndexMap.h>

#include <dplyr/BoolResult.h>

#include <dplyr/DataFrameSubsetVisitors.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/DataFrameJoinVisitors.h>

#include <dplyr/train.h>

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

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool convert = false) {
  int n = x.size();

  bool null_x = Rf_isNull(x.names()), null_y = Rf_isNull(y.names());
  if (null_x && !null_y) {
    return no_because("x does not have names, but y does");
  } else if (null_y && !null_x) {
    return no_because("y does not have names, but x does");
  } else if (null_x && null_y) {
    return compatible_data_frame_nonames(x, y, convert);
  }

  CharacterVector names_x = x.names();
  CharacterVector names_y = y.names();

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

  IntegerVector orders = r_match(names_x, names_y);

  for (int i = 0; i < n; i++) {
    SymbolString name = names_x[i];
    SEXP xi = x[i], yi = y[orders[i] - 1];
    boost::scoped_ptr<SubsetVectorVisitor> vx(subset_visitor(xi, name));
    boost::scoped_ptr<SubsetVectorVisitor> vy(subset_visitor(yi, name));

    std::stringstream ss;
    bool compatible = convert ?
                      vx->is_compatible(vy.get(), ss, name) :
                      vx->is_same_type(vy.get(), ss, name);

    if (!compatible) {
      if (ss.str() == "") {
        ss << "Incompatible type for column `"
           << name.get_utf8_cstring()
           << "`: x " << vx->get_r_type()
           << ", y " << vy->get_r_type();
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
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true, true);
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

// [[Rcpp::export]]
DataFrame union_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true, true);
  Set set(visitors);

  train_insert(set, x.nrows());
  train_insert_right(set, y.nrows());

  return visitors.subset(set, get_class(x));
}

// [[Rcpp::export]]
DataFrame intersect_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }
  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;

  DataFrameJoinVisitors visitors(x, y, x.names(), x.names(), true, true);
  Set set(visitors);

  train_insert(set, x.nrows());

  std::vector<int> indices;
  int n_y = y.nrows();
  for (int i = 0; i < n_y; i++) {
    Set::iterator it = set.find(-i - 1);
    if (it != set.end()) {
      indices.push_back(*it);
      set.erase(it);
    }
  }

  return visitors.subset(indices, get_class(x));
}

// [[Rcpp::export]]
DataFrame setdiff_data_frame(DataFrame x, DataFrame y) {
  BoolResult compat = compatible_data_frame(x, y, true, true);
  if (!compat) {
    stop("not compatible: %s", compat.why_not());
  }

  typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set;
  DataFrameJoinVisitors visitors(y, x, y.names(), y.names(), true, true);
  Set set(visitors);

  train_insert(set, y.nrows());

  std::vector<int> indices;

  int n_x = x.nrows();
  for (int i = 0; i < n_x; i++) {
    if (!set.count(-i - 1)) {
      set.insert(-i - 1);
      indices.push_back(-i - 1);
    }
  }

  return visitors.subset(indices, get_class(x));
}
