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
     
    void SplitApplyCombine::train(){
        if( !index_map.visitors || index_map.visitors != &visitors) {
            index_map = ChunkIndexMap( &visitors ) ; 
            trained = false ;  
        }
        if( !trained ){
            int n = data_frame.nrows() ;
            for( int i=0; i<n; i++) {
                index_map[i].push_back(i) ;
            }
            trained = true ;        
        }
    }
    
    List SplitApplyCombine::process(){ 
        // might not have been trained
        train() ;
        
        // train the map, i.e. collect incides for each chunk
        int nvisitors = visitors.size() ;
        int nresults = results.size() ;
        
        List output( nvisitors + nresults ) ;
        CharacterVector names( nvisitors + nresults ) ;
        
        // collect the visitors data
        ChunkIndexMap::iterator it ;
        int m = index_map.size() ;
        for( int i=0; i<nvisitors; i++){
            output[i] = visitors.get(i)->copy(index_map); 
            names[i]  = visitors.name(i) ;
        }
        
        // collect the results data
        for( int i=0; i<nresults; i++){
            names[i + nvisitors] = results.name(i) ;
            output[i + nvisitors] = results.get(i)->process( index_map ) ;
        }  
        output.attr("names") = names ; 
        output.attr("class") = "data.frame" ;
        output.attr("row.names") = IntegerVector::create( IntegerVector::get_na(), -m) ;
        return output ;
    }
       
    void SplitApplyCombine::add_result( Rcpp::String name, Result* result ){
        results.add_result( name, result ) ;  
    }
    
    void SplitApplyCombine::group_by( Rcpp::CharacterVector groups ){
        int n = groups.size() ;
        for( int i=0; i<n; i++)
            visitors.add_visitor( groups[i] ) ;    
    }
    
    #define HANDLE_CASE(__RTYPE__,__CLASS__)                 \
    case __RTYPE__:                                          \
    if( na_rm ) {                                            \
        result = new __CLASS__<__RTYPE__, true>(variable) ;  \
    } else {                                                 \
        result = new __CLASS__<__RTYPE__, false>(variable) ; \
    }                                                        \
    add_result( name, result ) ;                             \
    break                     
     
    #define RESULT_NUMBERS(__CLASS__,__FUN__)                                                      \
    void SplitApplyCombine::add_result( Rcpp::String name, __CLASS__ ## _Proxy proxy ){            \
        SEXP variable = data_frame[proxy.variable] ;                                               \
        bool na_rm = proxy.na_rm ;                                                                 \
        Result* result ;                                                                           \
        switch( TYPEOF(variable) ){                                                                \
            HANDLE_CASE(INTSXP,__CLASS__)  ;                                                       \
            HANDLE_CASE(REALSXP,__CLASS__) ;                                                       \
        default:                                                                                   \
            break ;                                                                                \
        }                                                                                          \
    }                                                                                              \
    void SplitApplyCombine::add_##__FUN__( Rcpp::String name, Rcpp::String variable, bool na_rm ){ \
        add_result( name, __CLASS__##_Proxy(variable, na_rm ) ) ;                                  \
    }
    
    #define RESULT_NUMBERS_AND_STRING(__CLASS__,__FUN__)                                           \
    void SplitApplyCombine::add_result( Rcpp::String name, __CLASS__ ## _Proxy proxy ){            \
        SEXP variable = data_frame[proxy.variable] ;                                               \
        bool na_rm = proxy.na_rm ;                                                                 \
        Result* result ;                                                                           \
        switch( TYPEOF(variable) ){                                                                \
            HANDLE_CASE(INTSXP,__CLASS__)  ;                                                       \
            HANDLE_CASE(REALSXP,__CLASS__) ;                                                       \
            HANDLE_CASE(STRSXP,__CLASS__) ;                                                        \
        default:                                                                                   \
            break ;                                                                                \
        }                                                                                          \
    }                                                                                              \
    void SplitApplyCombine::add_##__FUN__( Rcpp::String name, Rcpp::String variable, bool na_rm ){ \
        add_result( name, __CLASS__##_Proxy(variable, na_rm ) ) ;                                  \
    }
    
    RESULT_NUMBERS(Mean,mean)
    RESULT_NUMBERS(Var,var)
    RESULT_NUMBERS(Sd,sd)
    RESULT_NUMBERS(Sum,sum)
    RESULT_NUMBERS_AND_STRING(Min,min)
    RESULT_NUMBERS_AND_STRING(Max,max)
    
    #undef HANDLE_CASE
    #undef RESULT_NUMBERS
    #undef RESULT_NUMBERS_AND_STRING
    
    void SplitApplyCombine::add_count(Rcpp::String name){
        add_result( name, count() ) ;    
    } 
    
    void SplitApplyCombine::add_reduce_call( Rcpp::String target, Rcpp::Language call){
        add_result( target, new CallReducer( call, data_frame ) ) ;  
    }
    
    void SplitApplyCombine::add_result( Rcpp::String name, Reducer_Proxy proxy ){
        SEXP variable = data_frame[proxy.variable] ;
        switch( TYPEOF(variable)){
        case INTSXP: 
            add_result( name, new DelayedReducer<INTSXP>( proxy.fun, proxy.variable, variable ) ) ;
            break;
        case REALSXP: 
            add_result( name, new DelayedReducer<REALSXP>( proxy.fun, proxy.variable, variable ) );
            break;
        case STRSXP: 
            add_result( name, new DelayedReducer<STRSXP>( proxy.fun, proxy.variable, variable ) ) ;
            break;
        default:
            break;
        }
    }
    void SplitApplyCombine::add_reduce( Rcpp::String name, Rcpp::Function fun, Rcpp::String variable ){
        add_result( name, reduce( fun, variable ) ) ; 
    }
        
}   

RCPP_MODULE(SplitApplyCombineModule){
    using namespace dplyr ;
    
    class_<SplitApplyCombine>( "SplitApplyCombine" )
        .constructor<DataFrame>()
        
        .method( "group_by", &SplitApplyCombine::group_by )
    
        .method( "train", &SplitApplyCombine::train )
        
        .method( "process" , &SplitApplyCombine::process )
        
        .method( "add_count" , &SplitApplyCombine::add_count )
        .method( "add_mean"  , &SplitApplyCombine::add_mean )
        .method( "add_sum"   , &SplitApplyCombine::add_sum )
        .method( "add_min"   , &SplitApplyCombine::add_min )
        .method( "add_max"   , &SplitApplyCombine::add_max )
        .method( "add_var"   , &SplitApplyCombine::add_var )
        .method( "add_sd"    , &SplitApplyCombine::add_sd )
        .method( "add_reduce", &SplitApplyCombine::add_reduce )
        .method( "add_reduce_call", &SplitApplyCombine::add_reduce_call )
    ; 
    
}

