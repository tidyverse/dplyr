#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

SEXP and_calls( List args ){
    int ncalls = args.size() ;
    if( !ncalls ) return Rf_ScalarLogical(TRUE) ;
    
    Rcpp::Armor<SEXP> res( args[0] ) ;
    SEXP and_symbol = Rf_install( "&" ) ;
    for( int i=1; i<ncalls; i++)
        res = Rcpp_lang3( and_symbol, res, args[i] ) ;
    return res ;
}

DataFrame subset( DataFrame data, LogicalVector test, CharacterVector select, CharacterVector classes ){
    DataFrameVisitors visitors( data, select ) ;
    return visitors.subset(test, classes ) ;
}

DataFrame filter_grouped( const GroupedDataFrame& gdf, List args, Environment env){
    // a, b, c ->  a & b & c
    Language call = and_calls( args ) ;
    
    DataFrame data = gdf.data() ;
    int nrows = data.nrows() ;
    LogicalVector test = no_init(nrows);
    
    LogicalVector g_test ;
    CallProxy call_proxy( call, data ) ;
    int ngroups = gdf.ngroups() ;
    for( int i=0; i<ngroups; i++){
        Index_0_based indices = gdf.group(i) ;
        g_test  = call_proxy.get( indices );
        
        int chunk_size = indices.size() ;
        for( int j=0; j<chunk_size; j++){
            test[ indices[j] ] = g_test[j] ;  
        }
    }
    
    DataFrame res = subset( data, test, data.names(), classes_grouped() ) ;
    res.attr( "vars")   = gdf.attr("vars") ;
            
    return res ;
}

SEXP filter_not_grouped( DataFrame df, List args, Environment env){
    // a, b, c ->  a & b & c
    Language call = and_calls( args ) ;
    
    // replace the symbols that are in the data frame by vectors from the data frame
    // and evaluate the expression
    LogicalVector test = CallProxy( call, df).get( Everything() ) ;
    
    DataFrame res = subset( df, test, df.names(), classes_not_grouped() ) ;
    return res ;
}

// [[Rcpp::export]]
SEXP filter_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return filter_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return filter_not_grouped( df, args, env) ;   
    }
}

