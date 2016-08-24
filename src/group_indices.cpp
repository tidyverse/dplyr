#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
IntegerVector grouped_indices_grouped_df_impl( GroupedDataFrame gdf ){
  int n=gdf.nrows() ;
  IntegerVector res = no_init(n) ;
  int ngroups = gdf.ngroups() ;
  GroupedDataFrameIndexIterator it = gdf.group_begin() ;
  for(int i=0; i<ngroups; i++, ++it){
    SlicingIndex index = *it ;
    int n_index = index.size() ;
    for( int j=0; j<n_index; j++){
      res[ index[j] ] = i + 1 ;
    }
  }
  return res ;
}

// [[Rcpp::export]]
IntegerVector grouped_indices_impl( DataFrame data, ListOf<Symbol> symbols ){
  int nsymbols = symbols.size() ;
  if( nsymbols == 0 )
    return rep(1, data.nrows()) ;
  CharacterVector vars(nsymbols) ;
  for( int i=0; i<nsymbols; i++){
    vars[i] = PRINTNAME(symbols[i]) ;

    const char* name = vars[i] ;
    SEXP v  ;
    try {
      v = data[name] ;
    } catch(...){
      stop( "unknown column '%s'", name ) ;
    }
    if( !white_list(v) || TYPEOF(v) == VECSXP ){
      stop( "cannot group column %s, of class '%s'", name, get_single_class(v) ) ;
    }
  }

  DataFrameVisitors visitors(data, vars) ;
  ChunkIndexMap map( visitors ) ;
  int n = data.nrows() ;
  train_push_back( map, n ) ;

  DataFrame labels = DataFrameSubsetVisitors(data, vars).subset( map, "data.frame") ;
  IntegerVector labels_order = OrderVisitors(labels).apply() ;

  labels = DataFrameSubsetVisitors(labels).subset(labels_order, "data.frame" ) ;

  int ngroups = map.size() ;

  IntegerVector res = no_init(n) ;

  std::vector<const std::vector<int>* > chunks(ngroups) ;
  ChunkIndexMap::const_iterator it = map.begin() ;
  for( int i=0; i<ngroups; i++, ++it){
    chunks[i] = &it->second ;
  }

  for( int i=0; i<ngroups; i++){
    int idx = labels_order[i] ;
    const std::vector<int>& v = *chunks[idx] ;

    int n_index = v.size() ;
    for( int j=0; j<n_index; j++){
      res[ v[j] ] = i+1 ;
    }
  }

  return res ;
}
