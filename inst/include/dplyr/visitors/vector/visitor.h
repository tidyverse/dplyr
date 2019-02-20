#ifndef dplyr_visitor_H
#define dplyr_visitor_H

#include <dplyr/visitors/vector/VectorVisitor.h>

namespace dplyr {

inline VectorVisitor* visitor(SEXP vec);

}

#endif
