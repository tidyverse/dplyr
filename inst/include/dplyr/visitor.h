#ifndef dplyr_visitor_H
#define dplyr_visitor_H

namespace dplyr {

    inline VectorVisitor* visitor( SEXP vec ){
        
        if( Rf_isMatrix( vec ) ){
            switch( TYPEOF(vec) ){
            case CPLXSXP: return new MatrixColumnVisitor<CPLXSXP>( vec ) ;
            case INTSXP: return new MatrixColumnVisitor<INTSXP>( vec ) ;
            case REALSXP: return new MatrixColumnVisitor<REALSXP>( vec ) ;
            case LGLSXP: return new MatrixColumnVisitor<LGLSXP>( vec ) ;
            case STRSXP: return new MatrixColumnVisitor<STRSXP>( vec ) ;
            case VECSXP: return new MatrixColumnVisitor<VECSXP>( vec ) ;
            default:
                return 0 ;
            }
        }
        
        switch( TYPEOF(vec) ){
            case CPLXSXP:
                return new VectorVisitorImpl<CPLXSXP>( vec ) ;
            case INTSXP: 
                if( Rf_inherits(vec, "factor" ))
                    return new FactorVisitor( vec ) ;
                return new VectorVisitorImpl<INTSXP>( vec ) ;
            case REALSXP:
                return new VectorVisitorImpl<REALSXP>( vec ) ;
            case LGLSXP:  return new VectorVisitorImpl<LGLSXP>( vec ) ;
            case STRSXP:  return new VectorVisitorImpl<STRSXP>( vec ) ;
                
            case VECSXP:  {
                    if( Rf_inherits( vec, "data.frame" ) ){
                        return new DataFrameColumnVisitor(vec) ;    
                    }
                    if( Rf_inherits( vec, "POSIXlt" )) {
                        stop( "POSIXlt not supported" ) ;    
                    }
                    return new VectorVisitorImpl<VECSXP>( vec ) ;
            }
            default: break ;
        }
        
        // should not happen
        return 0 ;
    }
             
}

#endif
