#ifndef dplyr_tools_FilteredListOf_H
#define dplyr_tools_FilteredListOf_H

namespace Rcpp {
    
    template <typename T>
    class StrictListOf {
    public: 
        
        StrictListOf(SEXP data_) : data(data_){
          int n = data.size() ;
          for( int i=0; i<n; i++){
            if( !is<T>(data[i]) ){
              std::stringstream s ;
              s << "object at index " 
                << i 
                << " not compatible with class "
                << DEMANGLE(T) ;
              stop( s.str() ) ;
            }
          }
        }
        
        T operator[](int i) const { 
            return as<T>( data[i] ) ;
        }
        
        int size() const { 
            return data.size() ; 
        }
        
    private:
        List data ;
    } ;
}

#endif
