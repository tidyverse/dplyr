#include <dplyr.h>

using namespace Rcpp;

template <int RTYPE>
IntegerVector get_dim( const Matrix<RTYPE>& x){
  if (!x.hasAttribute("dim"))
    stop("`x` is not a matrix");

  IntegerVector dim = x.attr("dim");
  if (dim.size() != 2)
    stop("`x` is not a matrix");

  return dim ;
}

template <int RTYPE>
CharacterVector get_names( const Matrix<RTYPE>& x, int nc){
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

template <int RTYPE>
List copy_columns( const Matrix<RTYPE>& m ){
  int ncol = m.ncol(), nrow = m.nrow() ;

  CharacterVector names = get_names(m, ncol) ;

  List out(ncol) ;
  for(int j=0; j<ncol; j++) {
    typename Matrix<RTYPE>::ConstColumn column( m.column(j) ) ;
    Vector<RTYPE> vec( column.begin(), column.end() )  ;
    copy_most_attributes( vec, m ) ;
    Rf_setAttrib( vec, R_DimSymbol, R_NilValue ) ;
    out[j] = vec ;
  }

  out.attr("names") = names ;
  out.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  out.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);

  return out ;
}


// [[Rcpp::export]]
List matrixToDataFrame(SEXP x) {
  SEXPTYPE type = TYPEOF(x);

  switch(type){
    case LGLSXP:  return copy_columns<LGLSXP>(x) ;  break ;
    case INTSXP:  return copy_columns<INTSXP>(x) ;  break ;
    case REALSXP: return copy_columns<REALSXP>(x) ; break ;
    case CPLXSXP: return copy_columns<CPLXSXP>(x) ; break ;
    case STRSXP:  return copy_columns<STRSXP>(x) ;  break ;
    case VECSXP:  return copy_columns<VECSXP>(x) ;  break ;
    default:
    break ;
  }
  stop( "data type not handled" ) ;
  return List() ;

}
