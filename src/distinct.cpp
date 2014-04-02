#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
SEXP distinct_impl( DataFrame df ){
    DataFrameVisitors visitors(df) ;
    
    std::vector<int> indices ;
    VisitorSetIndexSet<DataFrameVisitors> set(visitors) ;
    
    int n = df.nrows() ;
    for( int i=0; i<n; i++){
        if( !set.count(i) ) {
            indices.push_back(i) ;
            set.insert(i) ;
        }
    }
    return visitors.subset(indices, df.attr("class") ); 
}

