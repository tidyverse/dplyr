#include <Rcpp.h>
using namespace Rcpp;

IntegerVector get_dim( const RObject& x){
  if (!x.hasAttribute("dim"))
    stop("`x` is not a matrix");

  IntegerVector dim = x.attr("dim");
  if (dim.size() != 2)
    stop("`x` is not a matrix");

  return dim ;
}

CharacterVector get_names( const RObject& x, const IntegerVector& dim){
  int nc = dim[1] ;
  if( x.hasAttribute("dimnames") ){
    List dimnames = x.attr("dimnames") ;
    try {
      CharacterVector res( dimnames[1] ) ;
      return res ;
    } catch(...){}
  }

  CharacterVector names( nc ) ;
  for( int i=0; i<nc; i++){
    names[i] = tfm::format( "V%d", (i+1) ) ;
  }
  return names ;
}

// [[Rcpp::export]]
List matrixToDataFrame(RObject x) {
  SEXPTYPE type = TYPEOF(x);

  IntegerVector dim = get_dim(x) ;
  CharacterVector names = get_names(x, dim) ;

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
      case CPLXSXP:
        COMPLEX(col)[i] = COMPLEX(x)[offset + i];
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

  out.attr("names") = names ;
  out.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);

  return out;
}
