#ifndef dplyr_dplyr_H
#define dplyr_dplyr_H

#include <Rcpp.h>
#include <solaris/solaris.h>

using namespace Rcpp ;

// borrowed from Rcpp11
#ifndef RCPP_DEBUG_OBJECT
    #define RCPP_DEBUG_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( "str"), OBJ ), R_GlobalEnv ) ) ;    
#endif

#ifndef RCPP_INSPECT_OBJECT
    #define RCPP_INSPECT_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( ".Internal"), Rf_lang2( Rf_install( "inspect" ), OBJ ) ), R_GlobalEnv ) ) ;
#endif

#include <boost/scoped_ptr.hpp>
#include <boost/functional/hash.hpp>

#ifndef dplyr_hash_map
    #if defined(_WIN32)
        #define dplyr_hash_map RCPP_UNORDERED_MAP
    #else
        #include <boost/unordered_map.hpp>
        #define dplyr_hash_map boost::unordered_map
    #endif
#endif

#ifndef dplyr_hash_set
    #if defined(_WIN32)
        #define dplyr_hash_set RCPP_UNORDERED_SET
    #else
        #include <boost/unordered_set.hpp>
        #define dplyr_hash_set boost::unordered_set
    #endif
#endif


#include <tools/tools.h>

namespace dplyr {
    class Result ;
    class ResultSet ;   
    class Reducer_Proxy ;
    class DataFrameVisitors ;
    class DataFrameJoinVisitors ;
    class LazySubsets ;
    template <typename OUT, int INPUT_RTYPE> class Reducer ; 
    const char* get_single_class(SEXP x) ;
}
dplyr::Result* get_handler( SEXP, const dplyr::LazySubsets&, const Environment& ) ;
bool can_simplify(SEXP) ;

void assert_all_white_list(const DataFrame&) ;
inline SEXP as_symbol(SEXP x) {
    return Rf_install( CHAR(x) );
}
inline SEXP shared_SEXP(SEXP x){
    SET_NAMED(x, 2 );  
    return x ;  
}
void check_supported_type(SEXP) ;
SEXP pairlist_shallow_copy(SEXP) ;
void copy_attributes(SEXP, SEXP) ;

// currently [[Rcpp::register]] does nothing.
//
// I'd like it to generate the boiler plate code
// that is in init.cpp and registration.h
//
// [[Rcpp::register]]
DataFrame build_index_cpp( DataFrame data ) ;

SEXP get_time_classes() ;
SEXP get_date_classes() ;

CharacterVector dfloc(List) ;

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int) ;

// [[Rcpp::register]]
void registerHybridHandler( const char* , HybridHandler ) ;

#include <dplyr/white_list.h>
#include <dplyr/check_supported_type.h>
#include <dplyr/visitor_set/visitor_set.h>
#include <dplyr/DataFrameVisitorsIndexSet.h>
#include <dplyr/DataFrameVisitorsIndexMap.h>
#include <dplyr/BoolResult.h>

#include <dplyr/EmptySubset.h>
#include <dplyr/FullDataFrame.h>
#include <dplyr/GroupedDataFrame.h>
#include <dplyr/tbl_cpp.h>
#include <dplyr/comparisons.h>
#include <dplyr/VectorVisitor.h>
#include <dplyr/OrderVisitor.h>
#include <dplyr/VectorVisitorImpl.h>
#include <dplyr/DataFrameVisitors.h>
#include <dplyr/OrderVisitorImpl.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/JoinVisitorImpl.h>
#include <dplyr/DataFrameJoinVisitors.h>
#include <dplyr/Order.h>
#include <dplyr/SummarisedVariable.h>
#include <dplyr/Result/all.h>
#include <dplyr/vector_class.h>
#include <dplyr/Gatherer.h>
#include <dplyr/Replicator.h>
#include <dplyr/Collecter.h>
#include <dplyr/NamedListAccumulator.h>

#include <dplyr/registration.h>

#endif
