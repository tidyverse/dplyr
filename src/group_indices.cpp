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
    CharacterVector vars(nsymbols) ;
    for( int i=0; i<nsymbols; i++){
        vars[i] = PRINTNAME(symbols[i]) ;

        const char* name = vars[i] ;
        SEXP v  ;
        try{ 
            v = data[name] ;
        } catch(...){
           std::stringstream s ;
           s << "unknown column '"
             << name
             << "'"; 
           stop(s.str()); 
        }
        if( !white_list(v) || TYPEOF(v) == VECSXP ){
            std::stringstream ss ;
            ss << "cannot group column "
               << name
               <<", of class '"
               << get_single_class(v)
               << "'" ;
            stop(ss.str()) ;
        }
    }

    DataFrameVisitors visitors(data, vars) ;
    ChunkIndexMap map( visitors ) ;
    int n = data.nrows() ;
    train_push_back( map, n ) ;

    int ngroups = map.size() ;

    IntegerVector res = no_init(n) ;
    
    ChunkIndexMap::const_iterator it = map.begin() ;
    for( int i=0; i<ngroups; i++, ++it){
        const std::vector<int>& v = it->second ;
        int n_index = v.size() ;
        for( int j=0; j<n_index; j++){
            res[ v[j] ] = i+1 ;    
        }
    }

    return res ;
}

