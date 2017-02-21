#ifndef dplyr_dplyr_extract_column_H
#define dplyr_dplyr_extract_column_H

#include <tools/SymbolString.h>

namespace dplyr {

  SymbolString extract_column(SEXP, const Environment&);

}

#endif // #ifndef dplyr_dplyr_extract_column_H
