#ifndef dplyr_dplyr_Groups_H
#define dplyr_dplyr_Groups_H

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf);
void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf);

void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf);
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf);

SEXP strip_group_attributes(SEXP df);

#endif // #ifndef dplyr_dplyr_Groups_H
