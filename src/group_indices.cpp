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

