#define COMPILING_DPLYR
#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
                
namespace dplyr {

    OrderVisitor* order_visitor( SEXP vec, bool ascending ){
        if( ascending )
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , true>( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, true>( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , true>( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , true>( vec ) ;
                default: break ;
            }
        else 
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , false>( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, false>( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , false>( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , false>( vec ) ;
                default: break ;
            }
        
        // should not happen
        return 0 ;
    }
    
    Gatherer* gatherer( CallProxy& proxy, const GroupedDataFrame& gdf ){
        Index_0_based indices = gdf.group(0);
        Shield<SEXP> first( proxy.get(indices) ) ; 
        switch( TYPEOF(first) ){
            case INTSXP:  return new GathererImpl<INTSXP> ( first, indices, proxy, gdf ) ;
            case REALSXP: return new GathererImpl<REALSXP>( first, indices, proxy, gdf ) ;
            case LGLSXP:  return new GathererImpl<LGLSXP> ( first, indices, proxy, gdf ) ;
            case STRSXP:  return new GathererImpl<STRSXP> ( first, indices, proxy, gdf ) ;
            default: break ;
        }
        // should not happen, but if it does, we should handle it
        return 0; 
    }
        
    JoinVisitor* join_visitor( SEXP left, SEXP right ){
        if( TYPEOF(left) != TYPEOF(right) ) 
            stop( "cannot create join visitor from incompatible types" ) ;
        switch( TYPEOF(left) ){
            case INTSXP:  return new JoinVisitorImpl<INTSXP> ( left, right ) ;
            case REALSXP: return new JoinVisitorImpl<REALSXP>( left, right ) ;
            case LGLSXP:  return new JoinVisitorImpl<LGLSXP> ( left, right ) ;
            case STRSXP:  return new JoinVisitorImpl<STRSXP> ( left, right ) ;
            default: break ;
        }
        return 0 ;
    }

}
