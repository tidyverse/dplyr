#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

template <typename Data, typename Subsets>
SEXP summarise_grouped(const DataFrame& df, const LazyDots& dots){
    Data gdf(df) ;
    
    int nexpr = dots.size() ;
    int nvars = gdf.nvars() ;
    check_not_groups(dots, gdf);
    NamedListAccumulator<Data> accumulator ;

    int i=0;
    for( ; i<nvars; i++){
        accumulator.set( PRINTNAME(gdf.symbol(i)), shared_SEXP(gdf.label(i)) ) ;
    }

    Subsets subsets(gdf) ;
    Shelter<SEXP> __ ;
    for( int k=0; k<nexpr; k++, i++ ){
        Rcpp::checkUserInterrupt() ;
        const Lazy& lazy = dots[k] ;
        const Environment& env = lazy.env() ;
        
        Shield<SEXP> expr_(lazy.expr()) ; SEXP expr = expr_ ;
        boost::scoped_ptr<Result> res( get_handler( expr, subsets, env ) );
        
        // if we could not find a direct Result
        // we can use a GroupedCallReducer which will callback to R
        if( !res ) {
            res.reset( new GroupedCallReducer<Data, Subsets>( lazy.expr(), subsets, env) );
        }
        SEXP result = __( res->process(gdf) ) ;
        accumulator.set( lazy.name(), result );
        subsets.input( Symbol(lazy.name()), SummarisedVariable(result) ) ;
        
    }

    return summarised_grouped_tbl_cpp<Data>(accumulator, gdf );
}


SEXP summarise_not_grouped(DataFrame df, const LazyDots& dots){
    int nexpr = dots.size() ;
    if( nexpr == 0) return DataFrame() ;
    
    LazySubsets subsets( df ) ;
    std::vector<SEXP> results ;
    NamedListAccumulator<DataFrame> accumulator ;

    Rcpp::Shelter<SEXP> __ ;
    for( int i=0; i<nexpr; i++){
        Rcpp::checkUserInterrupt() ;
        
        const Lazy& lazy = dots[i] ;
        Environment env = lazy.env() ;
        Shield<SEXP> expr_(lazy.expr()) ; SEXP expr = expr_ ;
        
        boost::scoped_ptr<Result> res( get_handler( expr, subsets, env ) ) ;
        Rprintf( "res = %p, type=%s\n", (Result*)res, DEMANGLE(*res) ) ;
        SEXP result ;
        if(res) {
            result = __(res->process( FullDataFrame(df) )) ;
        } else {
            result = __(CallProxy( lazy.expr(), subsets, env).eval()) ;
        }
        if( Rf_length(result) != 1 ){
            stop( "expecting result of length one, got : %d", Rf_length(result) ) ;
        }
        accumulator.set(lazy.name(), result);
        subsets.input( Symbol(lazy.name()), result ) ;
    }

    return tbl_cpp( accumulator, 1 ) ;
}

// [[Rcpp::export]]
SEXP summarise_impl( DataFrame df, LazyDots dots){
    check_valid_colnames(df) ;
    if( is<RowwiseDataFrame>(df) ){
        return summarise_grouped<RowwiseDataFrame, LazyRowwiseSubsets>( df, dots);
    } else if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped<GroupedDataFrame, LazyGroupedSubsets>( df, dots);
    } else {
        return summarise_not_grouped( df, dots) ;
    }
}

