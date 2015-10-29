#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List matrixToDataFrame(RObject x) {
  SEXPTYPE type = TYPEOF(x);

  if (!x.hasAttribute("dim"))
    stop("`x` is not a matrix");

  IntegerVector dim = x.attr("dim");
  if (dim.size() != 2)
    stop("`x` is not a matrix");

  int nrow = dim[0], ncol = dim[1];

  List out = List(ncol);
  for (int j = 0; j < ncol; ++j) {
    out[j] = Rf_allocVector(type, nrow);
    SEXP col = out[j];
    Rf_copyMostAttrib(x, col);
    int offset = j * nrow;
    for (int i = 0; i < nrow; ++i) {
      switch(type) {
      case LGLSXP:
      case INTSXP:
        INTEGER(col)[i] = INTEGER(x)[offset + i];
        break;
      case REALSXP:
        REAL(col)[i] = REAL(x)[offset + i];
        break;
      case STRSXP:
        SET_STRING_ELT(col, i, STRING_ELT(x, offset + i));
        break;
      case VECSXP:
        SET_VECTOR_ELT(col, i, VECTOR_ELT(x, offset + i));
        break;
      }
    }
  }

  if (x.hasAttribute("dimnames")) {
    List dimnames = x.attr("dimnames");
    out.attr("names") = dimnames[1];
  }

  out.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);

  return out;
}
