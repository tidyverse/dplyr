#ifndef dplyr_dplyr_H
#define dplyr_dplyr_H

#include <Rcpp.h>
#include <dplyr/workarounds/static_assert.h>
#include <solaris/solaris.h>
#include <dplyr/config.h>
#include <dplyr/workarounds.h>

using namespace Rcpp;

#include <tools/all_na.h>

// borrowed from Rcpp11
#ifndef RCPP_DEBUG_OBJECT
  #define RCPP_DEBUG_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( "str"), OBJ ), R_GlobalEnv ) );
#endif

#ifndef RCPP_INSPECT_OBJECT
  #define RCPP_INSPECT_OBJECT(OBJ) Rf_PrintValue( Rf_eval( Rf_lang2( Rf_install( ".Internal"), Rf_lang2( Rf_install( "inspect" ), OBJ ) ), R_GlobalEnv ) );
#endif

#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
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

#include <dplyr/ForwardDeclarations.h>

void assert_all_white_list(const DataFrame&);

inline SEXP shared_SEXP(SEXP x) {
  SET_NAMED(x, 2);
  return x;
}

SEXP pairlist_shallow_copy(SEXP p);
void copy_attributes(SEXP out, SEXP data);
void copy_most_attributes(SEXP out, SEXP data);

CharacterVector dfloc(List);
SEXP shallow_copy(const List& data);

typedef dplyr::Result* (*HybridHandler)(SEXP, const dplyr::LazySubsets&, int);

#if defined(COMPILING_DPLYR)
  DataFrame build_index_cpp(DataFrame data);
  void registerHybridHandler(const char*, HybridHandler);
  SEXP get_time_classes();
  SEXP get_date_classes();
#endif

#include <dplyr/registration.h>

#include <dplyr/DataFrameAble.h>
#include <dplyr/CharacterVectorOrderer.h>
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
#include <dplyr/SubsetVectorVisitor.h>
#include <dplyr/OrderVisitor.h>
#include <dplyr/VectorVisitorImpl.h>
#include <dplyr/SubsetVectorVisitorImpl.h>
#include <dplyr/DataFrameVisitors.h>
#include <dplyr/MultipleVectorVisitors.h>
#include <dplyr/DataFrameSubsetVisitors.h>
#include <dplyr/DataFrameColumnSubsetVisitor.h>
#include <dplyr/MatrixColumnSubsetVectorVisitor.h>
#include <dplyr/MatrixColumnVisitor.h>
#include <dplyr/DataFrameColumnVisitor.h>
#include <dplyr/subset_visitor.h>
#include <dplyr/visitor.h>
#include <dplyr/OrderVisitorImpl.h>
#include <dplyr/JoinVisitor.h>
#include <dplyr/JoinVisitorImpl.h>
#include <dplyr/DataFrameJoinVisitors.h>
#include <dplyr/Order.h>
#include <dplyr/SummarisedVariable.h>
#include <dplyr/Hybrid.h>
#include <dplyr/Result/all.h>
#include <dplyr/vector_class.h>
#include <dplyr/Gatherer.h>
#include <dplyr/Replicator.h>
#include <dplyr/Collecter.h>
#include <dplyr/NamedListAccumulator.h>
#include <dplyr/train.h>

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf);
void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf);

void check_not_groups(const LazyDots& dots, const GroupedDataFrame& gdf);
void check_not_groups(const LazyDots& dots, const RowwiseDataFrame& gdf);

SEXP strip_group_attributes(SEXP df);


#endif
