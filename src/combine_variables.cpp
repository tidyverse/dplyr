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

// [[Rcpp::export]]
IntegerVector combine_vars(CharacterVector vars, ListOf<IntegerVector> xs) {
  std::map<int, std::string> selected;
  if (xs.size() == 0)
    return IntegerVector::create();

  // If first component is negative, pre-fill with existing vars
  if (vector_sign(xs[0]) == -1) {
    for (int j = 0; j < vars.size(); ++j) {
      selected[j + 1] = vars[j];
    }
  }

  for (int i = 0; i < xs.size(); ++i) {
    IntegerVector x = xs[i];
    int sign = vector_sign(x);

    if (sign == 0)
      stop("Each argument must yield either position or negative integers");

    if (sign == 1) {
      bool has_names = x.attr("names") != R_NilValue;
      if (has_names) {
        // Named vectors always override existing
        CharacterVector names = as<CharacterVector>(x.attr("names"));
        for (int j = 0; j < x.size(); ++j) {
          selected[x[j] + 1] = names[j];
        }
      } else {
        for (int j = 0; j < x.size(); ++j) {
          int pos = x[j];
          if (pos < 1 || pos > vars.size())
            stop("Position must be between 0 and n");

          // Add default name, if not all ready present
          if (!selected.count(pos))
            selected[pos] = vars[pos - 1];
        }
      }
    } else {
      for (int j = 0; j < x.size(); ++j) {
        selected.erase(-x[j]);
      }
    }
  }

  int m = selected.size();
  IntegerVector out(m);
  CharacterVector out_names(m);

  std::map<int, std::string>::iterator selected_it = selected.begin();
  int i = 0;
  for(; selected_it != selected.end(); ++selected_it, ++i) {
    out[i] = selected_it->first;
    out_names[i] = selected_it->second;
  }
  out.attr("names") = out_names;
  return out;
}

