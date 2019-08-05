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

// [[Rcpp::export(rng = false)]]
dplyr::BoolResult equal_data_frame(Rcpp::DataFrame x, Rcpp::DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false) {
  // dplyr::BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
  // if (!compat) return compat;

  typedef dplyr::VisitorSetIndexMap<dplyr::DataFrameJoinVisitors, std::vector<int> > Map;
  dplyr::SymbolVector x_names(Rf_getAttrib(x, dplyr::symbols::names));
  dplyr::DataFrameJoinVisitors visitors(x, y, x_names, x_names, true, true);
  Map map(visitors);

  // train the map in both x and y
  int nrows_x = x.nrows();
  int nrows_y = y.nrows();

  if (nrows_x != nrows_y)
    return dplyr::no_because("Different number of rows");
  if (x.size() == 0)
    return dplyr::yes();

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

    return dplyr::no_because(Rcpp::CharacterVector::create(Rcpp::String(ss.str(), CE_UTF8)));
  }

  if (ok && ignore_row_order) return dplyr::yes();

  if (!ignore_row_order) {
    for (int i = 0; i < nrows_x; i++) {
      if (!visitors.equal(i, -i - 1)) {
        return dplyr::no_because("Same row values, but different order");
      }
    }
  }

  return dplyr::yes();
}

