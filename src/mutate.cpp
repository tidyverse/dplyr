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

SEXP structure_mutate( CallProxy& call_proxy, const DataFrame& df, const CharacterVector& results_names ){
    int n = call_proxy.nsubsets() ;
    
    List out(n) ;
    CharacterVector names(n) ;
    
    CharacterVector input_names = df.names() ;
    int ncolumns = df.size() ;
    int i=0 ;
    for( ; i<ncolumns; i++){
        out[i] = call_proxy.get_variable(input_names[i]) ;
        SET_NAMED( out[i], 2 );
        names[i] = input_names[i] ;
    }
    for( int k=0; i<n; k++ ){
        String name = results_names[k] ;
        
        if( ! any( input_names.begin(), input_names.end(), name.get_sexp() ) ){
            SEXP x   = call_proxy.get_variable( name ) ; 
            out[i]   = x ;
            SET_NAMED( out[i], 2 );
            names[i] = name ;
            i++ ;
        }
    }
    out.attr("class") = "data.frame" ;
    out.attr("row.names") = IntegerVector::create( 
        IntegerVector::get_na(), -df.nrows()
    ) ;
    out.names() = names;
    
    return out.asSexp() ;    
}

SEXP mutate_grouped(GroupedDataFrame gdf, List args, Environment env){
    DataFrame df = gdf.data() ;
    
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    CallProxy call_proxy(df) ;
    Shelter<SEXP> __ ;
    
    for( int i=0; i<nexpr; i++){
        call_proxy.set_call( args[i] );
        Gatherer* gather = gatherer( call_proxy, gdf );
        SEXP res = __( gather->collect() ) ;
        delete gather ;
        call_proxy.input( results_names[i], res ) ;
    }
    
    return structure_mutate( call_proxy, df, results_names) ;
}

SEXP mutate_not_grouped(DataFrame df, List args, Environment env){
    Shelter<SEXP> __ ;
    
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    CallProxy call_proxy(df) ;
    for( int i=0; i<nexpr; i++){
        call_proxy.set_call( args[i] );
        
        // we need to protect the SEXP, that's what the Shelter does
        SEXP res = __( call_proxy.get() ) ;
        call_proxy.input( results_names[i], res ) ;
        
    }
    
    return structure_mutate(call_proxy, df, results_names ) ;
}


// [[Rcpp::export]]
SEXP mutate_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return mutate_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return mutate_not_grouped( df, args, env) ;   
    }
}

