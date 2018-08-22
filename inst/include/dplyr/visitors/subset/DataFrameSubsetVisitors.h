#ifndef DPLY_VISITORS_SUBSET_DataFrameSubsetVisitors_H
#define DPLY_VISITORS_SUBSET_DataFrameSubsetVisitors_H

#include <tools/utils.h>
#include <tools/set_rownames.h>
#include <tools/is_lubridate_unsupported.h>
#include <tools/bad.h>
#include <dplyr/visitors/subset/column_subset.h>

namespace dplyr {

class DataFrameSubsetVisitors {
private:
  DataFrame data;

public:
  DataFrameSubsetVisitors(const DataFrame& data_): data(data_) {}

  inline int size() const {
    return data.size();
  }

  template <typename Index>
  DataFrame subset_all(const Index& index) const {
    return dataframe_subset<Index>(data, index, get_class(data));
  }

  template <typename Index>
  SEXP subset_one(int i, const Index& index) const {
    return column_subset(data[i], index);
  }

};

}

#endif

