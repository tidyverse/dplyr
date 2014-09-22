#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

template <typename Data, typename Subsets>
SEXP summarise_grouped(const DataFrame& df, List args, const DataDots& dots){
    Data gdf(df) ;
    
    int nexpr = dots.size() ;
    int nvars = gdf.nvars() ;
    CharacterVector results_names = args.names() ;
    check_not_groups(results_names, gdf);
    NamedListAccumulator<Data> accumulator ;

    int i=0;
    for( ; i<nvars; i++){
        accumulator.set( PRINTNAME(gdf.symbol(i)), shared_SEXP(gdf.label(i)) ) ;
    }

    Subsets subsets(gdf) ;
    Shelter<SEXP> __ ;
    for( int k=0; k<nexpr; k++, i++ ){
        Rcpp::checkUserInterrupt() ;
        
        Environment env = dots.envir(k) ;

        Result* res = get_handler( args[dots.expr_index(k)], subsets, env ) ;
        
        // if we could not find a direct Result
        // we can use a GroupedCallReducer which will callback to R
        if( !res ) res = new GroupedCallReducer<Data, Subsets>( args[dots.expr_index(k)], subsets, env) ;
        
        
        SEXP result = __( res->process(gdf) ) ;
        SEXP name = results_names[dots.expr_index(k)] ;
        accumulator.set( name, result );
        subsets.input( Symbol(name), SummarisedVariable(result) ) ;
        delete res;
    }

    return summarised_grouped_tbl_cpp<Data>(accumulator, gdf );
}


SEXP summarise_not_grouped(DataFrame df, List args, const DataDots& dots){
    int nexpr = dots.size() ;
    if( nexpr == 0) return DataFrame() ;
    
    CharacterVector names = args.names();

    LazySubsets subsets( df ) ;
    std::vector<SEXP> results ;
    std::vector<SEXP> result_names ;
    NamedListAccumulator<DataFrame> accumulator ;

    Rcpp::Shelter<SEXP> __ ;
    for( int i=0; i<nexpr; i++){
        Rcpp::checkUserInterrupt() ;
        
        SEXP name = names[dots.expr_index(i)] ;
        Environment env = dots.envir(i) ;
        Result* res = get_handler( args[i], subsets, env ) ;
        
        SEXP result ;
        if(res) {
            result = __(res->process( FullDataFrame(df) )) ;
        } else {
            result = __(CallProxy( args[dots.expr_index(i)], subsets, env).eval()) ;
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
    check_valid_rownames(df) ;
    DataDots dots(env) ;
    if( is<RowwiseDataFrame>(df) ){
        return summarise_grouped<RowwiseDataFrame, LazyRowwiseSubsets>( df, args, dots);
    } else if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped<GroupedDataFrame, LazyGroupedSubsets>( df, args, dots);
    } else {
        return summarise_not_grouped( df, args, dots) ;
    }
}

