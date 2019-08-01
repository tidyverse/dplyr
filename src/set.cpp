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
dplyr::BoolResult compatible_data_frame_nonames(Rcpp::DataFrame x, Rcpp::DataFrame y, bool convert) {
  int n = x.size();
  if (n != y.size())
    return dplyr::no_because(tfm::format("different number of columns : %d x %d", n, y.size()));

  if (convert) {
    for (int i = 0; i < n; i++) {
      try {
        boost::scoped_ptr<dplyr::JoinVisitor> v(
          dplyr::join_visitor(
            Column(x[i], dplyr::SymbolString("x")), Column(y[i], dplyr::SymbolString("y")), true, true
          )
        );
      } catch (...) {
        return dplyr::no_because("incompatible");
      }
    }
  } else {
    for (int i = 0; i < n; i++) {
      SEXP xi = x[i], yi = y[i];
      if (TYPEOF(xi) != TYPEOF(yi))
        return dplyr::no_because("incompatible types");

      if (TYPEOF(xi) == INTSXP) {
        if (Rf_inherits(xi, "factor") && Rf_inherits(yi, "factor")) {
          if (dplyr::same_levels(xi, yi)) continue;
          return dplyr::no_because("factors with different levels");
        }

        if (Rf_inherits(xi, "factor")) return dplyr::no_because("cannot compare factor and integer");
        if (Rf_inherits(yi, "factor")) return dplyr::no_because("cannot compare factor and integer");

      }
    }
  }

  return dplyr::yes();

}

bool same_factor_levels(SEXP x, SEXP y, std::stringstream& ss, const dplyr::SymbolString& name) {
  bool res = dplyr::same_levels(x, y);
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

bool type_same(SEXP x, SEXP y, std::stringstream& ss, const dplyr::SymbolString& name) {
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
    return dplyr::get_single_class(x);
  } else if (Rf_inherits(x, "Date")) {
    return "Date";
  } else if (Rf_isFactor(x)) {
    return dplyr::get_single_class(x);
  } else {
    return dplyr::get_single_class(x);
  }
}

// [[Rcpp::export(rng = false)]]
dplyr::BoolResult compatible_data_frame(Rcpp::DataFrame x, Rcpp::DataFrame y, bool ignore_col_order = true, bool convert = false) {
  int n = x.size();

  Rcpp::Shield<SEXP> x_names(Rf_getAttrib(x, dplyr::symbols::names));
  Rcpp::Shield<SEXP> y_names(Rf_getAttrib(y, dplyr::symbols::names));

  bool null_x = Rf_isNull(x_names);
  bool null_y = Rf_isNull(y_names);
  if (null_x && !null_y) {
    return dplyr::no_because("x does not have names, but y does");
  } else if (null_y && !null_x) {
    return dplyr::no_because("y does not have names, but x does");
  } else if (null_x && null_y) {
    return compatible_data_frame_nonames(x, y, convert);
  }

  Rcpp::CharacterVector names_x(x_names);
  Rcpp::CharacterVector names_y(y_names);

  Rcpp::CharacterVector names_y_not_in_x = setdiff(names_y, names_x);
  Rcpp::CharacterVector names_x_not_in_y = setdiff(names_x, names_y);

  if (!ignore_col_order) {
    if (names_y_not_in_x.size() == 0 && names_x_not_in_y.size() == 0) {
      // so the names are the same, check if they are in the same order
      for (int i = 0; i < n; i++) {
        if (names_x[i] != names_y[i]) {
          return dplyr::no_because("Same column names, but different order");
        }
      }
    }
  }

  Rcpp::CharacterVector why;
  if (names_y_not_in_x.size()) {
    std::stringstream ss;
    ss << "Cols in y but not x: " << dplyr::collapse_utf8(names_y_not_in_x, ", ", "`") << ". ";
    why.push_back(Rcpp::String(ss.str(), CE_UTF8));
  }

  if (names_x_not_in_y.size()) {
    std::stringstream ss;
    ss << "Cols in x but not y: " << dplyr::collapse_utf8(names_x_not_in_y, ", ", "`") << ". ";
    why.push_back(Rcpp::String(ss.str(), CE_UTF8));
  }

  if (why.length() > 0) return dplyr::no_because(why);

  Rcpp::Shield<SEXP> orders(dplyr::r_match(names_x, names_y));
  int* p_orders = INTEGER(orders);

  for (int i = 0; i < n; i++) {
    dplyr::SymbolString name = names_x[i];
    SEXP xi = x[i], yi = y[p_orders[i] - 1];

    std::stringstream ss;
    bool compatible = convert ? type_compatible(xi, yi) : type_same(xi, yi, ss, name);

    if (!compatible) {
      if (ss.str() == "") {
        ss << "Incompatible type for column `"
           << name.get_utf8_cstring()
           << "`: x " << type_describe(xi)
           << ", y " << type_describe(yi);
      }

      why.push_back(Rcpp::String(ss.str(), CE_UTF8));
    }

  }

  if (why.length() > 0) return dplyr::no_because(why);
  return dplyr::yes();
}

// [[Rcpp::export(rng = false)]]
dplyr::BoolResult equal_data_frame(Rcpp::DataFrame x, Rcpp::DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false) {
  dplyr::BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
  if (!compat) return compat;

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

