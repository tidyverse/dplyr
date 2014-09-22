#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef dplyr_hash_set<SEXP> SymbolSet ;

inline SEXP empty_subset( const DataFrame& df, CharacterVector columns, CharacterVector classes ){
    DataFrameVisitors visitors(df, columns) ;
    return visitors.subset( EmptySubset(), classes) ;
}

SEXP assert_correct_filter_subcall(SEXP x, const SymbolSet& set, const Environment& env){
    switch(TYPEOF(x)){
    case LANGSXP: return x ;
    case SYMSXP:
        {
            if( set.count(x) ) return x ;
            
            // look in the environment
            SEXP res = Rf_findVar( x, env ) ;
            if( res == R_UnboundValue ){
                if( x == Rf_install("T") ){
                    return Rf_ScalarLogical(TRUE) ;
                } else if( x == Rf_install("F") ){
                    return Rf_ScalarLogical(FALSE) ;    
                }
                
                std::stringstream s ;
                s << "unknown column : " << CHAR(PRINTNAME(x)) ;
                stop(s.str());
            }
            return res ;
        }
    default:
        break ;
    }
    stop("incompatible expression in filter") ;
    return x ; // never happens
}

SEXP and_calls( List args, const SymbolSet& set, const Environment& env ){
    int ncalls = args.size() ;
    if( !ncalls ) {
        stop("incompatible input") ;
    }

    Rcpp::Armor<SEXP> res( assert_correct_filter_subcall(args[0], set, env) ) ;
    SEXP and_symbol = Rf_install( "&" ) ;
    for( int i=1; i<ncalls; i++)
        res = Rcpp_lang3( and_symbol, res, assert_correct_filter_subcall(args[i], set, env) ) ;
    
    return res ;
}

void check_filter_result(const LogicalVector& test, int n){
    if( test.size() != n ) {
        std::stringstream s ;
        s << "incorrect length ("
          << test.size()
          << "), expecting: "
          << n ;
        stop( s.str() ) ;
    }
}

inline SEXP check_filter_logical_result(SEXP tmp){
    if( TYPEOF(tmp) != LGLSXP ){
        stop( "filter condition does not evaluate to a logical vector. " ) ; 
    }
    return tmp ;
}

DataFrame filter_grouped_single_env( const GroupedDataFrame& gdf, const List& args, const Environment& env){
    typedef GroupedCallProxy<GroupedDataFrame, LazyGroupedSubsets> Proxy ; 
    
    const DataFrame& data = gdf.data() ;
    CharacterVector names = data.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }

    // a, b, c ->  a & b & c
    Call call( and_calls( args, set, env ) ) ;

    int nrows = data.nrows() ;
    LogicalVector test(nrows, TRUE);

    LogicalVector g_test ;
    Proxy call_proxy( call, gdf, env ) ;

    int ngroups = gdf.ngroups() ;
    GroupedDataFrame::group_iterator git = gdf.group_begin() ;
    for( int i=0; i<ngroups; i++, ++git){
        SlicingIndex indices = *git ;
        int chunk_size = indices.size() ;
        
        g_test = check_filter_logical_result( call_proxy.get( indices ) ) ;
        if( g_test.size() == 1 ){
            int val = g_test[0] == TRUE ;
            for( int j=0; j<chunk_size; j++){
                test[ indices[j] ] = val ;
            }
        } else {
            check_filter_result(g_test, chunk_size ) ;
            for( int j=0; j<chunk_size; j++){
                if( g_test[j] != TRUE ) test[ indices[j] ] = FALSE ;
            }
        }
    }
    DataFrame res = subset( data, test, names, classes_grouped<GroupedDataFrame>() ) ;
    res.attr( "vars")   = data.attr("vars") ;

    return res ;
}

// version of grouped filter when contributions to ... come from several environment
DataFrame filter_grouped_multiple_env( const GroupedDataFrame& gdf, const List& args, const DataDots& dots){
    const DataFrame& data = gdf.data() ;
    CharacterVector names = data.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }

    int nrows = data.nrows() ;
    LogicalVector test(nrows, TRUE);

    LogicalVector g_test ;

    for( int k=0; k<dots.size(); k++){
        Rcpp::checkUserInterrupt() ;
    
        Call call( (SEXP)args[dots.expr_index(k)] ) ;
        GroupedCallProxy<GroupedDataFrame> call_proxy( call, gdf, dots.envir(k) ) ;
        int ngroups = gdf.ngroups() ;
        GroupedDataFrame::group_iterator git = gdf.group_begin() ;
        for( int i=0; i<ngroups; i++, ++git){
            SlicingIndex indices = *git ;
            int chunk_size = indices.size() ;

            g_test  = check_filter_logical_result(call_proxy.get( indices ));
            if( g_test.size() == 1 ){
                if( g_test[0] != TRUE ){
                    for( int j=0; j<chunk_size; j++){
                        test[indices[j]] = FALSE ;    
                    }
                }
            } else {
                check_filter_result(g_test, chunk_size ) ;
                for( int j=0; j<chunk_size; j++){
                    if( g_test[j] != TRUE ){
                        test[ indices[j] ] = FALSE ;
                    }
                }
            }
        }
    }
    DataFrame res = subset( data, test, names, classes_grouped<GroupedDataFrame>() ) ;
    res.attr( "vars") = data.attr("vars") ;

    return res ;
}

DataFrame filter_grouped( const GroupedDataFrame& gdf, List args, const DataDots& dots){
    if( dots.single_env() ){
        return filter_grouped_single_env(gdf, args, dots.envir(0) ) ;
    } else {
        return filter_grouped_multiple_env(gdf,args,dots) ;
    }
}

bool combine_and(LogicalVector& test, const LogicalVector& test2){
    int n = test.size() ;
    if(n == 1) {
        test = test2 ;
    } else { 
        int n2 = test2.size() ;
        if( n2 == 1 ){
            if( !test2[0] ){
                return true ;
            }
        } else if( n2 == n) {
            for( int i=0; i<n; i++){
                test[i] = test[i] && test2[i] ;
            }
        } else {
            stop( "incompatible sizes" ) ;    
        }
    }
    return false;
}

SEXP filter_not_grouped( DataFrame df, List args, const DataDots& dots){
    CharacterVector names = df.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }
    if( dots.single_env() ){
        Environment env = dots.envir(0) ;
        // a, b, c ->  a & b & c
        Shield<SEXP> call( and_calls( args, set, env ) ) ;
        // replace the symbols that are in the data frame by vectors from the data frame
        // and evaluate the expression
        CallProxy proxy( (SEXP)call, df, env ) ;
        
        LogicalVector test = check_filter_logical_result(proxy.eval()) ;
        if( test.size() == 1){
            if( test[0] == TRUE ){
                return df ; 
            } else {
                return empty_subset(df, df.names(), classes_not_grouped()) ;    
            }
        } else {
            check_filter_result(test, df.nrows());
            DataFrame res = subset( df, test, df.names(), classes_not_grouped() ) ;
            return res ;
        }
    } else {
        int nargs = args.size() ;
        CallProxy first_proxy(args[0], df, dots.envir(0) ) ;
        LogicalVector test = check_filter_logical_result(first_proxy.eval()) ;
        if( test.size() == 1 ) {
            if( !test[0] ){
                return empty_subset(df, df.names(), classes_not_grouped() ) ;    
            }
        } else {
            check_filter_result(test, df.nrows());
        }
        
        for( int i=1; i<nargs; i++){
            Rcpp::checkUserInterrupt() ;
    
            LogicalVector test2 = check_filter_logical_result(CallProxy(args[i], df, dots.envir(i) ).eval()) ;
            if( combine_and(test, test2) ){
                return empty_subset(df, df.names(), classes_not_grouped() ) ;
            }
        }

        DataFrame res = subset( df, test, df.names(), classes_not_grouped() ) ;
        return res ;
    }
}

// [[Rcpp::export]]
SEXP filter_impl( DataFrame df, List args, Environment env){
    check_valid_colnames(df) ;
    assert_all_white_list(df) ;
    
    if( args.size() == 0 ) return df ;
    
    // special case
    if( args.size() == 1 && TYPEOF(args[0]) == LGLSXP){
        LogicalVector what = args[0] ;
        if( what.size() == 1 ){
            if( what[0] == TRUE ){
                return df ;   
            } else {
                return empty_subset( df, df.names(), is<GroupedDataFrame>(df) ? classes_grouped<GroupedDataFrame>() : classes_not_grouped() ) ;    
            }
        }
    }
    
    DataDots dots(env) ;
    
    if( is<GroupedDataFrame>( df ) ){
        return filter_grouped( GroupedDataFrame(df), args, dots);
    } else {
        return filter_not_grouped( df, args, dots) ;
    }
}

