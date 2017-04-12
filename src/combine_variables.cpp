#include "pch.h"
#include <Rcpp.h>
using namespace Rcpp;

int vector_sign(IntegerVector x) {
  bool pos = false, neg = false;

  int n = x.size();
  for (int i = 0; i < n; ++i) {
    if (x[i] < 0) neg = true;
    if (x[i] > 0) pos = true;

    if (neg && pos) break;
  }

  if (neg == pos) {
    // Either mixed, or all zeros
    return 0;
  } else if (neg) {
    return -1;
  } else {
    return 1;
  }
}

class VarList {

  std::vector<int> out_indx;
  std::vector<String> out_name;

  int find(int i) {
    std::vector<int>::iterator pos = std::find(out_indx.begin(), out_indx.end(), i);
    if (pos == out_indx.end()) {
      return -1;
    } else {
      return pos - out_indx.begin();
    }
  }

public:
  explicit VarList(int n) : out_indx(), out_name() {
    out_indx.reserve(n);
    out_name.reserve(n);
  }

  bool has(int i) {
    return find(i) != -1;
  }

  void add(int i, String name) {
    out_indx.push_back(i);
    out_name.push_back(name);
  }
  void remove(int i) {
    int pos = find(i);
    if (pos == -1) return;

    out_indx.erase(out_indx.begin() + pos);
    out_name.erase(out_name.begin() + pos);
  }
  void update(int i, String name) {
    int pos = find(i);
    if (pos == -1) {
      add(i, name);
    } else {
      out_name[pos] = name;
    }
  }

  operator SEXP() {
    IntegerVector out(out_indx.begin(), out_indx.end());
    CharacterVector out_names(out_name.begin(), out_name.end());
    out.names() = out_names;

    return out;
  }
};

// [[Rcpp::export]]
SEXP combine_vars(CharacterVector vars, ListOf<IntegerVector> xs) {
  VarList selected(vars.size());
  if (xs.size() == 0)
    return IntegerVector::create();

  // Workaround bug in ListOf<>; can't access attributes
  SEXP raw_names = Rf_getAttrib(xs, Rf_mkString("names"));
  CharacterVector xs_names;
  if (raw_names == R_NilValue) {
    xs_names = CharacterVector(xs.size(), "");
  } else {
    xs_names = raw_names;
  }

  // If first component is negative, pre-fill with existing vars
  if (vector_sign(xs[0]) == -1) {
    for (int j = 0; j < vars.size(); ++j) {
      selected.add(j + 1, vars[j]);
    }
  }

  for (int i = 0; i < xs.size(); ++i) {
    IntegerVector x = xs[i];
    if (x.size() == 0) continue;

    int sign = vector_sign(x);

    if (sign == 0)
      stop("Each argument must yield either positive or negative integers");

    if (sign == 1) {
      bool group_named = xs_names[i] != "";
      bool has_names = x.attr("names") != R_NilValue;
      if (group_named) {
        if (x.size() == 1) {
          selected.update(x[0], xs_names[i]);
        } else {
          // If the group is named, children are numbered sequentially
          for (int j = 0; j < x.size(); ++j) {
            std::stringstream out;
            out << xs_names[i] << j + 1;
            selected.update(x[j], out.str());
          }
        }
      } else if (has_names) {
        CharacterVector names = x.names();
        for (int j = 0; j < x.size(); ++j) {
          selected.update(x[j], names[j]);
        }
      } else {
        for (int j = 0; j < x.size(); ++j) {
          int pos = x[j];
          if (pos < 1 || pos > vars.size())
            stop("Position must be between 0 and n");

          // Add default name, if not all ready present
          if (!selected.has(pos))
            selected.update(pos, vars[pos - 1]);
        }
      }
    } else {
      for (int j = 0; j < x.size(); ++j) {
        selected.remove(-x[j]);
      }
    }
  }

  return selected;
}

