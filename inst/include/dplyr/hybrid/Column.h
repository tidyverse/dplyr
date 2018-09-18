#ifndef dplyr_hybrid_column_h
#define dplyr_hybrid_column_h

namespace dplyr {
namespace hybrid {

struct Column {
  SEXP data;
  bool is_desc;
};

}
}

#endif
