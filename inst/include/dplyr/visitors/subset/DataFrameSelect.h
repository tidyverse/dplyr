#ifndef DPLY_VISITORS_SUBSET_DataFrameSelect_H
#define DPLY_VISITORS_SUBSET_DataFrameSelect_H

#include <tools/utils.h>
#include <tools/bad.h>

namespace dplyr {

class DataFrameSelect {
private:
  List data;

public:
  DataFrameSelect(const DataFrame& data_, const SymbolVector& names): data(names.size()) {
    CharacterVector data_names = vec_names_or_empty(data_);
    IntegerVector indices = names.match_in_table(data_names);
    int n = indices.size();
    CharacterVector out_names(n);

    for (int i = 0; i < n; i++) {
      int pos = indices[i];
      if (pos == NA_INTEGER) {
        bad_col(names[i], "is unknown");
      }
      data[i] = data_[pos - 1];
      out_names[i] = data_names[pos - 1];
    }
    data.attr("names") = out_names;
    copy_class(data, data_);
  }

  DataFrameSelect(const DataFrame& data_, const IntegerVector& indices, bool check = true) : data(indices.size()) {
    CharacterVector data_names = vec_names_or_empty(data_);
    int n = indices.size();
    CharacterVector out_names(n);
    for (int i = 0; i < n; i++) {
      int pos = check ? check_range_one_based(indices[i], data_.size()) : indices[i];
      out_names[i] = data_names[pos - 1];
      data[i] = data_[pos - 1];
    }
    data.attr("names") = out_names;
    copy_class(data, data_);
  }

  inline operator SEXP() const {
    return data;
  }

};

}

#endif
