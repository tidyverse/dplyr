#ifndef dplyr_dplyr_H
#define dplyr_dplyr_H

#include <Rcpp.h>
using namespace Rcpp ;

#include <boost/functional/hash.hpp>
#include <boost/unordered_map.hpp>
#include <boost/unordered_set.hpp>

#include <tools/tools.h>

namespace dplyr {
    class Result ;
    class ResultSet ;   
    class Reducer_Proxy ;
    class DataFrameVisitors ;
    class DataFrameJoinVisitors ;
    template <typename OUT, int INPUT_RTYPE> class Reducer ; 
}

DataFrame build_index_cpp( DataFrame data ) ;

#include <dplyr/visitor_set/visitor_set.h>
#include <dplyr/DataFrameVisitorsIndexSet.h>
#include <dplyr/DataFrameVisitorsIndexMap.h>
#include <dplyr/BoolResult.h>

#include <dplyr/Index_0_based.h>
#include <dplyr/FullDataFrame.h>
#include <dplyr/GroupedDataFrame.h>
#include <dplyr/tbl_cpp.h>
#include <dplyr/comparisons.h>
#include <dplyr/VectorVisitor.h>
#include <dplyr/OrderVisitor.h>
#include <dplyr/DataFrameVisitors.h>
#include <dplyr/VectorVisitorImpl.h>
#include <dplyr/OrderVisitorImpl.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/JoinVisitorImpl.h>
#include <dplyr/DataFrameJoinVisitors.h>
#include <dplyr/Result/all.h>
#include <dplyr/Gatherer.h>
#include <dplyr/Order.h>

SEXP and_calls(List args) ;
DataFrame subset( DataFrame, LogicalVector, CharacterVector, CharacterVector ) ;

// borrowed from Rcpp11
#define RCPP_DEBUG_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( "str"), OBJ ), R_GlobalEnv ) ) ;    
#define RCPP_INSPECT_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( ".Internal"), Rf_lang2( Rf_install( "inspect" ), OBJ ) ), R_GlobalEnv ) ) ;

#endif
