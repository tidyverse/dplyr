#ifndef dplyr_dplyr_H
#define dplyr_dplyr_H

#include <Rcpp.h>
#include <solaris/solaris.h>
#include <dplyr/config.h>

using namespace Rcpp ;
#include <Rcpp/Benchmark/Timer.h>
#include <tools/all_na.h>
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
    std::string get_single_class(SEXP x) ;
    
    template <typename Index>
    DataFrame subset( DataFrame df, const Index& indices, CharacterVector classes) ;
    
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

inline SEXP pairlist_shallow_copy(SEXP p){
    Shield<SEXP> attr( Rf_cons(CAR(p), R_NilValue) ) ;
    SEXP q = attr ;
    SET_TAG(q, TAG(p)) ;
    p = CDR(p) ;
    while( !Rf_isNull(p) ){
        Shield<SEXP> s( Rf_cons(CAR(p), R_NilValue) ) ;
        SETCDR(q, s) ;
        q = CDR(q) ;
        SET_TAG(q, TAG(p)) ;
        p = CDR(p) ;
    }
    return attr ;   
}

inline void copy_attributes(SEXP out, SEXP data){
    SEXP att = ATTRIB(data) ;
    if( !Rf_isNull(att) ){
        SET_ATTRIB( out, pairlist_shallow_copy(ATTRIB(data)) ) ;
    }
    SET_OBJECT( out, OBJECT(data) );
}

// same as copy_attributes but without names
inline void copy_most_attributes(SEXP out, SEXP data){
    copy_attributes(out,data) ;
    Rf_setAttrib( out, R_NamesSymbol, R_NilValue ) ;
}
    
CharacterVector dfloc(List) ;
SEXP shallow_copy(const List& data) ;

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int) ;

#if defined(COMPILING_DPLYR)
    DataFrame build_index_cpp( DataFrame data ) ;
    void registerHybridHandler( const char* , HybridHandler ) ;
    SEXP get_time_classes() ;
    SEXP get_date_classes() ;
#endif

#include <dplyr/white_list.h>
#include <dplyr/check_supported_type.h>
#include <dplyr/visitor_set/visitor_set.h>
#include <dplyr/DataFrameVisitorsIndexSet.h>
#include <dplyr/DataFrameVisitorsIndexMap.h>
#include <dplyr/BoolResult.h>

#include <dplyr/EmptySubset.h>
#include <dplyr/FullDataFrame.h>
#include <dplyr/GroupedDataFrame.h>
#include <dplyr/RowwiseDataFrame.h>
#include <dplyr/tbl_cpp.h>
#include <dplyr/comparisons.h>
#include <dplyr/comparisons_different.h>
#include <dplyr/VectorVisitor.h>
#include <dplyr/OrderVisitor.h>
#include <dplyr/VectorVisitorImpl.h>
#include <dplyr/DataFrameVisitors.h>
#include <dplyr/MatrixColumnVisitor.h>
#include <dplyr/DataFrameColumnVisitor.h>
#include <dplyr/visitor.h>
#include <dplyr/OrderVisitorImpl.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/JoinVisitorImpl.h>
#include <dplyr/JoinFactorFactorVisitor_SameLevels.h>
#include <dplyr/DataFrameJoinVisitors.h>
#include <dplyr/Order.h>
#include <dplyr/SummarisedVariable.h>
#include <dplyr/Result/all.h>
#include <dplyr/vector_class.h>
#include <dplyr/Gatherer.h>
#include <dplyr/Replicator.h>
#include <dplyr/Collecter.h>
#include <dplyr/NamedListAccumulator.h>
#include <dplyr/train.h>

#include <dplyr/registration.h>

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf) ;
void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf) ;

void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf) ;
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf) ;

template <typename Data>
SEXP strip_group_attributes(Data df){
  Shield<SEXP> attribs( Rf_cons( dplyr::classes_not_grouped(), R_NilValue ) ) ;
  SET_TAG(attribs, Rf_install("class") ) ;

  SEXP p = ATTRIB(df) ;
  std::vector<SEXP> black_list(8) ;
  black_list[0] = Rf_install("indices") ;
  black_list[1] = Rf_install("vars") ;
  black_list[2] = Rf_install("index") ;
  black_list[3] = Rf_install("labels") ;
  black_list[4] = Rf_install("drop") ;
  black_list[5] = Rf_install("group_sizes") ;
  black_list[6] = Rf_install("biggest_group_size") ;
  black_list[7] = Rf_install("class") ;

  SEXP q = attribs ;
  while( ! Rf_isNull(p) ){
    SEXP tag = TAG(p) ;
    if( std::find( black_list.begin(), black_list.end(), tag ) == black_list.end() ){
      Shield<SEXP> s( Rf_cons( CAR(p), R_NilValue) ) ;
      SETCDR(q,s) ;
      q = CDR(q) ;
      SET_TAG(q, tag) ;
    }

    p = CDR(p) ;
  }
  return attribs ;
}

template <typename T>
CharacterVector names( const T& obj ){
    SEXP x = obj ;
    return Rf_getAttrib(x, Rf_install("names" ) ) ;    
}


#endif
