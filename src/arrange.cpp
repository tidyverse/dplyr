#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
List arrange_impl( DataFrame data, List args, DataDots dots ){
    if( dots.size() == 0 || data.nrows() == 0) return data ;
    assert_all_white_list(data) ;
    
    int nargs = args.size() ;
    List variables(nargs) ;
    LogicalVector ascending(nargs) ;
    Shelter<SEXP> __ ;
    
    for(int i=0; i<nargs; i++){
        SEXP call = args[i] ;
        bool is_desc = TYPEOF(call) == LANGSXP && Rf_install("desc") == CAR(call) ;
        
        CallProxy call_proxy(is_desc ? CADR(call) : call, data, dots.envir(i)) ;
        
        SEXP v = __(call_proxy.eval()) ;
        if( !white_list(v) || TYPEOF(v) == VECSXP ){
            std::stringstream ss ;
            ss << "cannot arrange column of class '"
               << get_single_class(v) 
               << "'" ;
            stop(ss.str()) ;
        }
        
        if( Rf_length(v) != data.nrows() ){
            std::stringstream s ;
            s << "incorrect size ("
              << Rf_length(v)
              << "), expecting :"
              << data.nrows() ;
            stop(s.str()) ;
        }
        variables[i] = v ;
        ascending[i] = !is_desc ;
    }
    OrderVisitors o(variables, ascending, nargs) ;
    IntegerVector index = o.apply() ;
    
    DataFrameVisitors visitors( data, data.names() ) ;
    List res = visitors.subset(index, data.attr("class") ) ;
    return res;
}

