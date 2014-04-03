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
        if( set.insert(i).second ){
            indices.push_back(i) ;    
        }
    }
    return visitors.subset(indices, df.attr("class") ); 
}

// [[Rcpp::export]]
SEXP union_impl( DataFrame x, DataFrame y ){
    CharacterVector by = x.names() ;
    DataFrameJoinVisitors visitors(x, y, by) ;     
    typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
    Set set(visitors) ;
    
    train_insert(set, x.nrows() ) ;
    train_insert_right(set, y.nrows() );
    std::vector<int> indices( set.begin(), set.end()) ;
    return visitors.subset( indices, x.attr("class")) ;
}

