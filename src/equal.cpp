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
using namespace dplyr ;

SEXP no_because( std::string msg ){
    LogicalVector res = Rf_ScalarLogical( FALSE ) ;
    res.attr("comment" ) = msg ;
    return res ;
}

SEXP yes(){
    return Rf_ScalarLogical(TRUE) ; 
}

template <typename VisitorSet>
bool all_same_types(const VisitorSet& vx, const VisitorSet& vy){
    int n = vx.size() ;
    for( int i=0; i<n; i++)
        if( typeid(vx.get(i)) != typeid(vy.get(i)) )
            return false ;
    return true ;
}

// [[Rcpp::export]]
SEXP equal_data_frame(DataFrame x, DataFrame y){
    int n = x.size() ;
    if( n != y.size() ) 
        return no_because( "not the same number of variables" ) ;
    
    int nrows = x.nrows() ;
    if( nrows != y.nrows() )
        return no_because( "different row sizes" );
    
    CharacterVector names_x = clone<CharacterVector>(x.names()) ; names_x.sort() ;
    CharacterVector names_y = clone<CharacterVector>(y.names()) ; names_y.sort() ;
    for( int i=0; i<n; i++) 
        if( names_x[i] != names_y[i] )
            return no_because( "not the same variable names. ") ; 
    
    DataFrameVisitors v_x( x, names_x );
    DataFrameVisitors v_y( x, names_y );
    if( ! all_same_types(v_x, v_y ) )
        return no_because( "different types" ) ;
    
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, int > Map ;
    DataFrameJoinVisitors visitors(x, y, names_x) ;
    Map map(visitors);  
    
    for( int i=0; i<nrows; i++) map[i]++ ;
    for( int i=0; i<nrows; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it == map.end() || it->second < 0 ) 
            return no_because( "different subset" ) ;
        else
            it->second-- ;
    }
    
    return yes() ;
}

// [[Rcpp::export]]
SEXP all_equal_data_frame( List args, Environment env ){
    int n = args.size() ;
    DataFrame x0 = Rf_eval( args[0], env) ;
    Armor<SEXP> test ;
    for( int i=1; i<n; i++){
        test = equal_data_frame( x0, Rf_eval( args[i], env ) ) ;
        if( ! LOGICAL(test)[0] )
            return test ;
    }
    return yes() ;
}
