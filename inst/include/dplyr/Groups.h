#ifndef dplyr_dplyr_Groups_H
#define dplyr_dplyr_Groups_H

#include <tools/Quosure.h>

#include <dplyr/data/GroupedDataFrame.h>
#include <dplyr/data/RowwiseDataFrame.h>
#include <dplyr/data/NaturalDataFrame.h>

void check_not_groups(const dplyr::QuosureList& quosures, const dplyr::GroupedDataFrame& gdf);
void check_not_groups(const dplyr::QuosureList& quosures, const dplyr::RowwiseDataFrame& gdf);
void check_not_groups(const dplyr::QuosureList& quosures, const dplyr::NaturalDataFrame& gdf);

#endif // #ifndef dplyr_dplyr_Groups_H
