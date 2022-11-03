#include "dplyr.h"

SEXP dplyr_reconstruct_data_frame(SEXP data, SEXP template_) {
  data = PROTECT(Rf_shallow_duplicate(data));

  // copy over all attributes except `names` and `row.names`
  SEXP attr_template = ATTRIB(template_);
  while(attr_template != R_NilValue) {
    SEXP tag = TAG(attr_template);
    if (tag != R_NamesSymbol && tag != R_RowNamesSymbol) {
      Rf_setAttrib(data, tag, CAR(attr_template));
    }
    attr_template = CDR(attr_template);
  }

  UNPROTECT(1);
  return data;
}
