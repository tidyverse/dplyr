#ifndef dplyr_subset_visitor_H
#define dplyr_subset_visitor_H

namespace dplyr {

    inline SubsetVectorVisitor* subset_visitor( SEXP vec ){

        if( Rf_isMatrix( vec ) ){
            switch( TYPEOF(vec) ){
            case CPLXSXP: return new MatrixColumnSubsetVisitor<CPLXSXP>( vec ) ;
            case INTSXP:  return new MatrixColumnSubsetVisitor<INTSXP>( vec ) ;
            case REALSXP: return new MatrixColumnSubsetVisitor<REALSXP>( vec ) ;
            case LGLSXP:  return new MatrixColumnSubsetVisitor<LGLSXP>( vec ) ;
            case STRSXP:  return new MatrixColumnSubsetVisitor<STRSXP>( vec ) ;
            case VECSXP:  return new MatrixColumnSubsetVisitor<VECSXP>( vec ) ;
            default: break;
            }

            stop( "unimplemented matrix type: %s", Rf_type2rstr(TYPEOF(vec)) ) ;
            return 0;
        }

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

        stop( "unimplemented vector type: %s", Rf_type2rstr(TYPEOF(vec)) ) ;
        return 0 ;
    }

}

#endif
