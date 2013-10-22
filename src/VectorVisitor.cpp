// Copyright (C) 2013    Romain Francois
// Copyright (C) 2013    Rice University
//
// This file is part of dplyr.
//
// dplyr is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// dplyr is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with dplyr.  If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
                
namespace dplyr {

    VectorVisitor* visitor( SEXP vec ){
        switch( TYPEOF(vec) ){
            case INTSXP:  return new VectorVisitorImpl<INTSXP>( vec ) ;
            case REALSXP: return new VectorVisitorImpl<REALSXP>( vec ) ;
            case LGLSXP:  return new VectorVisitorImpl<LGLSXP>( vec ) ;
            case STRSXP:  return new VectorVisitorImpl<STRSXP>( vec ) ;
            default: break ;
        }
        
        // should not happen
        return 0 ;
    }
    
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
