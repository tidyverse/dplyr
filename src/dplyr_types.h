#include <Rcpp.h>
using namespace Rcpp;

#include <tools/DotsOf.h>
#include <tools/LazyDots.h>
#include <dplyr/registration.h>
#include <dplyr/BoolResult.h>
#include <dplyr/GroupedDataFrame.h>

// avoid inclusion of package header file
#define dplyr_dplyr_H
