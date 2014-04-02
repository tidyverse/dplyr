#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

SEXP summarise_grouped(const GroupedDataFrame& gdf, List args, const DataDots& dots){
    DataFrame df = gdf.data() ;

    int nexpr = args.size() ;
    int nvars = gdf.nvars() ;
    CharacterVector results_names = args.names() ;
    check_not_groups(results_names, gdf);
    NamedListAccumulator accumulator ;

    int i=0;
    for( ; i<nvars; i++){
        accumulator.set( PRINTNAME(gdf.symbol(i)), shared_SEXP(gdf.label(i)) ) ;
    }

    LazyGroupedSubsets subsets(gdf) ;
    Shelter<SEXP> __ ;
    for( int k=0; k<nexpr; k++, i++ ){
        Rcpp::checkUserInterrupt() ;
        
        Environment env = dots.envir(k) ;

        Result* res = get_handler( args[k], subsets, env ) ;
        
        // if we could not find a direct Result
        // we can use a GroupedCalledReducer which will callback to R
        if( !res ) res = new GroupedCalledReducer( args[k], subsets, env) ;
        
        
        SEXP result = __( res->process(gdf) ) ;
        Rf_setAttrib( result, R_NamesSymbol, R_NilValue ) ;
        SEXP name = results_names[k] ;
        accumulator.set( name, result );
        subsets.input( Symbol(name), SummarisedVariable(result) ) ;
        delete res;
    }

    return summarised_grouped_tbl_cpp(accumulator, gdf );
}

SEXP summarise_not_grouped(DataFrame df, List args, const DataDots& dots){
    int nexpr = args.size() ;
    if( nexpr == 0) return DataFrame() ;
    
    CharacterVector names = args.names();

    LazySubsets subsets( df ) ;
    std::vector<SEXP> results ;
    std::vector<SEXP> result_names ;
    NamedListAccumulator accumulator ;

    Rcpp::Shelter<SEXP> __ ;
    for( int i=0; i<nexpr; i++){
        Rcpp::checkUserInterrupt() ;
        
        SEXP name = names[i] ;
        Environment env = dots.envir(i) ;
        Result* res = get_handler( args[i], subsets, env ) ;
        SEXP result ;
        if(res) {
            result = __(res->process( FullDataFrame(df) )) ;
        } else {
            result = __(CallProxy( args[i], subsets, env).eval()) ;
        }
        delete res ;
        if( Rf_length(result) != 1 ){
            std::stringstream s ;
            s << "expecting result of length one, got : "
              << Rf_length(result) ;
            stop(s.str()) ;
        }
        subsets.input( Symbol(name), result ) ;
        accumulator.set(name, result);
    }

    return tbl_cpp( accumulator, 1 ) ;
}

// [[Rcpp::export]]
SEXP summarise_impl( DataFrame df, List args, Environment env){
    DataDots dots(env) ;
    if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped( GroupedDataFrame(df), args, dots);
    } else {
        return summarise_not_grouped( df, args, dots) ;
    }
}

