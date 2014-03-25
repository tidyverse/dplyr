#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

SEXP select_not_grouped( const DataFrame& df, const CharacterVector& keep, CharacterVector new_names ){
  CharacterVector names = df.names() ;
  IntegerVector positions = match( keep, names ); 
  int n = keep.size() ; 
  List res(n) ;
  for( int i=0; i<n; i++){
    res[i] = df[ positions[i]-1 ] ;  
  }
  copy_attributes(res, df) ;
  res.names() = new_names ; 
  return res ; 
}

DataFrame select_grouped( GroupedDataFrame gdf, const CharacterVector& keep, CharacterVector new_names ){
  int n = keep.size() ;
  DataFrame copy = select_not_grouped( gdf.data(), keep, new_names );
  
  // handle vars  attribute : make a shallow copy of the list and alter 
  //   its names attribute
  List vars = shallow_copy( copy.attr("vars") ); 
  
  int nv = vars.size() ;
  for( int i=0; i<nv; i++){
    SEXP s = PRINTNAME(vars[i]) ;
    int j = 0; 
    for( ; j < n; j++){
      if( s == keep[j] ){
        vars[j] = Rf_install( CHAR(new_names[j]) );  
      }
    }
  }
  copy.attr("vars") = vars ;
    
  // hangle labels attribute
  //   make a shallow copy of the data frame and alter its names attributes
  if( !Rf_isNull( copy.attr("labels" ) ) ){
      
    DataFrame original_labels( copy.attr("labels" ) ) ;
    
    DataFrame labels = shallow_copy(original_labels) ;
    CharacterVector label_names = clone<CharacterVector>( labels.names() ) ;
    
    IntegerVector positions = match( label_names, keep ); 
    int nl = label_names.size() ;
    for( int i=0; i<nl; i++){
      label_names[positions[i]-1] = new_names[i] ;
    }
    labels.names() = label_names ;
    labels.attr("vars") = vars ;
    copy.attr("labels") = labels ;
  }
  return copy ;
}

// [[Rcpp::export]]
DataFrame select_impl( DataFrame df, CharacterVector vars ){
  if( is<GroupedDataFrame>(df) ){
    return select_grouped( GroupedDataFrame(df), vars, vars.names() ) ;  
  } else {
    return select_not_grouped(df, vars, vars.names() ) ;  
  }
}

