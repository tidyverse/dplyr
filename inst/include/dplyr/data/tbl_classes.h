#ifndef dplyr_data_tbl_classes_H
#define dplyr_data_tbl_classes_H

#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

namespace dplyr {

template <typename Data>
inline Rcpp::CharacterVector tbl_classes() ;

template <>
inline Rcpp::CharacterVector tbl_classes<GroupedDataFrame>() {
  return Rcpp::CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame");
}

template <>
inline Rcpp::CharacterVector tbl_classes<RowwiseDataFrame>() {
  return Rcpp::CharacterVector::create("rowwise_df", "tbl_df", "tbl", "data.frame");
}

template <>
inline Rcpp::CharacterVector tbl_classes<NaturalDataFrame>() {
  return Rcpp::CharacterVector::create("tbl_df", "tbl", "data.frame");
}

}

#endif
