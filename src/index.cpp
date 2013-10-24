#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
DataFrame build_index_cpp( DataFrame data ){
    CharacterVector vars = Rf_getAttrib( data.attr( "vars" ), R_NamesSymbol ) ;
    
    DataFrameVisitors visitors(data, vars) ;
    ChunkIndexMap map( visitors ) ;
    train_push_back( map, data.nrows() ) ;
    
    data.attr( "index" )  = get_all_second(map) ;
    data.attr( "labels" ) = visitors.subset(map, "data.frame" ) ;
    return data ;
}
