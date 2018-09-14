#ifndef dplyr_registration_H
#define dplyr_registration_H

#if defined(COMPILING_DPLYR)
SEXP get_time_classes();
SEXP get_date_classes();
#else
#include "dplyr_RcppExports.h"
#endif

#endif
