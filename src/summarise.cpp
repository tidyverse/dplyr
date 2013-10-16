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

#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef Result* (*ResultPrototype)(SEXP, const DataFrame&) ;
typedef std::unordered_map<SEXP,ResultPrototype> Result1_Map ;
  
#define MAKE_PROTOTYPE(__FUN__,__CLASS__)                               \
Result* __FUN__##_prototype( SEXP arg, const DataFrame& df ){           \
    const char* column_name = CHAR(PRINTNAME(arg)) ;                    \
    SEXP v = df[column_name] ;                                          \
    switch( TYPEOF(v) ){                                                \
        case INTSXP:  return new dplyr::__CLASS__<INTSXP,false>( v ) ;  \
        case REALSXP: return new dplyr::__CLASS__<REALSXP,false>( v ) ; \
        default: break ;                                                \
    }                                                                   \
    return 0 ;                                                          \
}
MAKE_PROTOTYPE(mean, Mean)
MAKE_PROTOTYPE(min, Min)
MAKE_PROTOTYPE(max, Max)
MAKE_PROTOTYPE(var, Var)
MAKE_PROTOTYPE(sd, Sd)
MAKE_PROTOTYPE(sum, Sum)

#define INSTALL_PROTOTYPE(__FUN__) prototypes[ Rf_install( #__FUN__ ) ] = __FUN__##_prototype ;

Result1_Map& get_1_arg_prototypes(){
    static Result1_Map prototypes ;
    if( !prototypes.size() ){ 
        INSTALL_PROTOTYPE(mean)
        INSTALL_PROTOTYPE(min)
        INSTALL_PROTOTYPE(max)
        INSTALL_PROTOTYPE(var)
        INSTALL_PROTOTYPE(sd)
        INSTALL_PROTOTYPE(sum)
    }
    return prototypes ;    
}

ResultPrototype get_1_arg(SEXP symbol){
    Result1_Map& prototypes = get_1_arg_prototypes() ;
    Result1_Map::iterator it = prototypes.find(symbol); 
    if( it == prototypes.end() ) return 0 ;
    return it->second ;
}

Result* get_result( SEXP call, const DataFrame& df){
    // no arguments
    int depth = Rf_length(call) ;
    if( depth == 1 && CAR(call) == Rf_install("n") )
        return new Count ;
    
    if( depth == 2 ){
        SEXP fun_symbol = CAR(call) ;
        SEXP arg1 = CADR(call) ;
        ResultPrototype reducer = get_1_arg( fun_symbol ) ;
        if( reducer ){
            Result* res = reducer( arg1, df ) ;
            if( res ) return res ;    
        }
    }
    
    return new CallReducer(call, df) ;
}

SEXP summarise_grouped(GroupedDataFrame gdf, List args, Environment env){
    DataFrame df = gdf.data() ;
    
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    Shelter<SEXP> __ ;
    std::vector<SEXP> results ;
    
    for( int i=0; i<nexpr; i++){
        Result* res = get_result( args[i], df ) ;
        results.push_back( __( res->process(gdf) ) );
        delete res ;
    }
    
    int nvars = gdf.nvars() ;
    
    List out(nexpr + nvars) ;
    CharacterVector names(nexpr + nvars) ;
    
    int i=0; 
    for( ; i<nvars; i++){
        out[i]      = gdf.label(i) ;
        SET_NAMED(out[i], 2) ;
        names[i]    = CHAR(PRINTNAME(gdf.symbol(i))) ;
    }
    for( int k=0; k<nexpr; k++, i++ ){
        String name = results_names[k] ;
        out[i]      = results[k] ;
        names[i]    = name ;
    }
    out.attr("class") = "data.frame" ;
    out.attr("row.names") = IntegerVector::create( 
        IntegerVector::get_na(), -gdf.ngroups()
    ) ;
    out.names() = names;
    
    return out.asSexp() ;
}

SEXP summarise_not_grouped(DataFrame df, List args, Environment env){
    Shelter<SEXP> __ ;
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    std::vector<SEXP> results ;
    for( int i=0; i<nexpr; i++){
        Result* res = get_result( args[i], df ) ;
        results.push_back( __( res->process( FullDataFrame(df) ) ) ) ; 
        delete res ;
    }
    
    List out(nexpr) ;
    CharacterVector names(nexpr) ;
    
    for( int k=0; k<nexpr; k++ ){
        String name = results_names[k] ;
        out[k]   = results[k] ; 
        names[k] = name ;
    }
    out.attr("class") = "data.frame" ;
    out.attr("row.names") = IntegerVector::create( 
        IntegerVector::get_na(), -1
    ) ;
    out.names() = names;
    
    return out.asSexp() ;
}

// [[Rcpp::export]]
SEXP summarise_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return summarise_not_grouped( df, args, env) ;   
    }
}

