#ifndef dplyr_subset_visitor_H
#define dplyr_subset_visitor_H

namespace dplyr {

inline SubsetVectorVisitor* subset_visitor_matrix( SEXP vec );
inline SubsetVectorVisitor* subset_visitor_vector( SEXP vec );

inline SubsetVectorVisitor* subset_visitor( SEXP vec ){
  if( Rf_isMatrix( vec ) ){
    return subset_visitor_matrix(vec) ;
  }
  else {
    return subset_visitor_vector(vec) ;
  }
}

inline SubsetVectorVisitor* subset_visitor_matrix( SEXP vec ){
  switch( TYPEOF(vec) ){
  case CPLXSXP: return new MatrixColumnSubsetVisitor<CPLXSXP>( vec ) ;
  case INTSXP:  return new MatrixColumnSubsetVisitor<INTSXP>( vec ) ;
  case REALSXP: return new MatrixColumnSubsetVisitor<REALSXP>( vec ) ;
  case LGLSXP:  return new MatrixColumnSubsetVisitor<LGLSXP>( vec ) ;
  case STRSXP:  return new MatrixColumnSubsetVisitor<STRSXP>( vec ) ;
  case VECSXP:  return new MatrixColumnSubsetVisitor<VECSXP>( vec ) ;
  default: break ;
  }

  stop("Unsupported matrix type %s", Rf_type2char(TYPEOF(vec))) ;
  return 0 ;
}

inline SubsetVectorVisitor* subset_visitor_vector( SEXP vec ){
  if( Rf_inherits(vec, "Date") ){
    return new DateSubsetVectorVisitor(vec) ;
  }

  switch( TYPEOF(vec) ){
  case CPLXSXP:
    return new SubsetVectorVisitorImpl<CPLXSXP>( vec ) ;
  case INTSXP:
    if( Rf_inherits(vec, "factor" ))
      return new SubsetFactorVisitor( vec ) ;
    return new SubsetVectorVisitorImpl<INTSXP>( vec ) ;
  case REALSXP: return new SubsetVectorVisitorImpl<REALSXP>( vec ) ;
  case LGLSXP:  return new SubsetVectorVisitorImpl<LGLSXP>( vec ) ;
  case STRSXP:  return new SubsetVectorVisitorImpl<STRSXP>( vec ) ;

  case VECSXP:  {
      if( Rf_inherits( vec, "data.frame" ) ){
        return new DataFrameColumnSubsetVisitor(vec) ;
      }
      if( Rf_inherits( vec, "POSIXlt" )) {
        stop( "POSIXlt not supported" ) ;
      }
      return new SubsetVectorVisitorImpl<VECSXP>( vec ) ;
    }
  default: break ;
  }

  // should not happen, safeguard against segfaults anyway
  stop("Unsupported vector type %s", Rf_type2char(TYPEOF(vec))) ;
  return 0 ;
}

}

#endif
