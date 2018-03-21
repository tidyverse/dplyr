#ifndef dplyr_registration_H
#define dplyr_registration_H

#include <dplyr/HybridHandler.h>

#if defined(COMPILING_DPLYR)

void build_index_cpp(DataFrame& data);
void registerHybridHandler(const char*, dplyr::HybridHandler);

SEXP get_time_classes();
SEXP get_date_classes();

#else

#include "dplyr_RcppExports.h"

#endif

#endif

