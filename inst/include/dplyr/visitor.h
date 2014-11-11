#ifndef dplyr_visitor_H
#define dplyr_visitor_H

namespace dplyr {

    inline VectorVisitor* visitor( SEXP vec ){
        switch( TYPEOF(vec) ){
            case CPLXSXP:
                return new VectorVisitorImpl<CPLXSXP>( vec ) ;
            case INTSXP: 
                if( Rf_inherits(vec, "factor" ))
                    return new FactorVisitor( vec ) ;
                if( Rf_inherits(vec, "Date") )
                    return new DateVisitor<INTSXP>(vec) ;
                if( Rf_inherits(vec, "POSIXct") )
                    return new POSIXctVisitor<INTSXP>(vec) ;
                return new VectorVisitorImpl<INTSXP>( vec ) ;
            case REALSXP:
                if( Rf_inherits( vec, "difftime" ) )
                    return new DifftimeVisitor<REALSXP>( vec ) ;
                if( Rf_inherits( vec, "Date" ) )
                    return new DateVisitor<REALSXP>( vec ) ;
                if( Rf_inherits( vec, "POSIXct" ) )
                    return new POSIXctVisitor<REALSXP>( vec ) ;
                return new VectorVisitorImpl<REALSXP>( vec ) ;
            case LGLSXP:  return new VectorVisitorImpl<LGLSXP>( vec ) ;
            case STRSXP:  return new VectorVisitorImpl<STRSXP>( vec ) ;
                
            case VECSXP:  {
                    if( Rf_inherits( vec, "data.frame" ) ){
                        return new DataFrameColumnVisitor(vec) ;    
                    } 
                    if( Rf_inherits( vec, "matrix" ) ){
                        stop( "matrices as columns are not supported" ) ;    
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
