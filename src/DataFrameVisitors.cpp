#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;

namespace dplyr {

    DataFrameVisitors::DataFrameVisitors( const DataFrame& data_, const CharacterVector& names) : 
        data(data_), 
        visitors()
    {
        std::string name ;
        int n = names.size() ;
        nvisitors = 0 ;
        std::vector<std::string> visitor_names_ ;
        for( int i=0; i<n; i++){
            name = (String)names[i] ;
            SEXP column = data[name] ;
            if( column != R_NilValue ){
                nvisitors++ ;
                visitor_names_.push_back( name ) ;
                visitors.push_back( visitor( column ) ) ;
            }
        }
        visitor_names = wrap( visitor_names_ );
    }
    
}
