#ifndef dplyr_visitor_impl_H
#define dplyr_visitor_impl_H

#include <dplyr/visitors/vector/VectorVisitorImpl.h>
#include <dplyr/visitors/vector/DataFrameColumnVisitor.h>
#include <dplyr/visitors/vector/MatrixColumnVisitor.h>

namespace dplyr {

inline VectorVisitor* visitor_matrix(SEXP vec);
inline VectorVisitor* visitor_vector(SEXP vec);
inline VectorVisitor* recycling_visitor_matrix(SEXP vec, int g, int n);
inline VectorVisitor* recycling_visitor_vector(SEXP vec, int g, int n);

inline VectorVisitor* visitor(SEXP vec) {
  if (Rf_isMatrix(vec)) {
    return visitor_matrix(vec);
  }
  else {
    return visitor_vector(vec);
  }
}

inline VectorVisitor* recycling_visitor(SEXP vec, int g, int n) {
  if (Rf_isMatrix(vec)) {
    return recycling_visitor_matrix(vec, g, n);
  }
  else {
    return recycling_visitor_vector(vec, g, n);
  }
}

inline VectorVisitor* visitor_matrix(SEXP vec) {
  switch (TYPEOF(vec)) {
  case CPLXSXP:
    return new MatrixColumnVisitor<CPLXSXP>(vec);
  case INTSXP:
    return new MatrixColumnVisitor<INTSXP>(vec);
  case REALSXP:
    return new MatrixColumnVisitor<REALSXP>(vec);
  case LGLSXP:
    return new MatrixColumnVisitor<LGLSXP>(vec);
  case STRSXP:
    return new MatrixColumnVisitor<STRSXP>(vec);
  case VECSXP:
    return new MatrixColumnVisitor<VECSXP>(vec);
  default:
    break;
  }

  stop("unsupported matrix type %s", Rf_type2char(TYPEOF(vec)));
}

inline VectorVisitor* recycling_visitor_matrix(SEXP vec, int g, int n) {
  switch (TYPEOF(vec)) {
  case CPLXSXP:
    return new RecyclingMatrixColumnVisitor<CPLXSXP>(vec, g, n);
  case INTSXP:
    return new RecyclingMatrixColumnVisitor<INTSXP>(vec, g, n);
  case REALSXP:
    return new RecyclingMatrixColumnVisitor<REALSXP>(vec, g, n);
  case LGLSXP:
    return new RecyclingMatrixColumnVisitor<LGLSXP>(vec, g, n);
  case STRSXP:
    return new RecyclingMatrixColumnVisitor<STRSXP>(vec, g, n);
  case VECSXP:
    return new RecyclingMatrixColumnVisitor<VECSXP>(vec, g, n);
  default:
    break;
  }

  stop("unsupported matrix type %s", Rf_type2char(TYPEOF(vec)));
}

inline VectorVisitor* visitor_vector(SEXP vec) {
  switch (TYPEOF(vec)) {
  case CPLXSXP:
    return new VectorVisitorImpl<CPLXSXP>(vec);
  case INTSXP:
    return new VectorVisitorImpl<INTSXP>(vec);
  case REALSXP:
    return new VectorVisitorImpl<REALSXP>(vec);
  case LGLSXP:
    return new VectorVisitorImpl<LGLSXP>(vec);
  case STRSXP:
    return new VectorVisitorImpl<STRSXP>(vec);
  case RAWSXP:
    return new VectorVisitorImpl<RAWSXP>(vec);

  case VECSXP: {
    if (Rf_inherits(vec, "data.frame")) {
      return new DataFrameColumnVisitor(vec);
    }
    if (Rf_inherits(vec, "POSIXlt")) {
      stop("POSIXlt not supported");
    }
    return new VectorVisitorImpl<VECSXP>(vec);
  }
  default:
    break;
  }

  // should not happen, safeguard against segfaults anyway
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(vec)));
}

inline VectorVisitor* recycling_visitor_vector(SEXP vec, int g, int n) {
  switch (TYPEOF(vec)) {
  case CPLXSXP:
    return new RecyclingVectorVisitorImpl<CPLXSXP>(vec, g, n);
  case INTSXP:
    return new RecyclingVectorVisitorImpl<INTSXP>(vec, g, n);
  case REALSXP:
    return new RecyclingVectorVisitorImpl<REALSXP>(vec, g, n);
  case LGLSXP:
    return new RecyclingVectorVisitorImpl<LGLSXP>(vec, g, n);
  case STRSXP:
    return new RecyclingVectorVisitorImpl<STRSXP>(vec, g, n);
  case RAWSXP:
    return new RecyclingVectorVisitorImpl<RAWSXP>(vec, g, n);

  case VECSXP:
  {
    // if (Rf_inherits(vec, "data.frame")) {
    // return new DataFrameColumnVisitor(vec);
    // }
    if (Rf_inherits(vec, "POSIXlt")) {
      stop("POSIXlt not supported");
    }
    return new RecyclingVectorVisitorImpl<VECSXP>(vec, g, n);
  }
  default:
    break;
  }

  // should not happen, safeguard against segfaults anyway
  stop("is of unsupported type %s", Rf_type2char(TYPEOF(vec)));
}


}

#endif
