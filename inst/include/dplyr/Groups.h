#ifndef dplyr_dplyr_Groups_H
#define dplyr_dplyr_Groups_H

#include <tools/LazyDots.h>
#include <tools/TidyQuote.h>

#include <dplyr/GroupedDataFrame.h>
#include <dplyr/RowwiseDataFrame.h>

// TODO: remove once transition to tidyeval is finished
void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf);
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf);

void check_not_groups(const TidyQuotes& quotes, const GroupedDataFrame& gdf);
void check_not_groups(const TidyQuotes& quotes, const RowwiseDataFrame& gdf);

SEXP strip_group_attributes(SEXP df);

#endif // #ifndef dplyr_dplyr_Groups_H
