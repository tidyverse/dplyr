#ifndef dplyr_tools_FilteredListOf_H
#define dplyr_tools_FilteredListOf_H

namespace Rcpp {
    
    template <typename T>
    struct IsValid {
        inline bool operator()(SEXP x){
            return is<T>(x) ;
        }
    } ;
    
    template <typename T>
    struct NULL_or_Is {
        inline bool operator()(SEXP x){
            return Rf_isNull(x) || is<DataFrame>(x) ;
        }    
    } ;

    template <typename T, typename Valid = IsValid<T> >
    class StrictListOf {
    public: 
        
        StrictListOf(SEXP data_, const char* msg = 0, Valid valid = Valid() ) : data(data_){
          int n = data.size() ;
          for( int i=0; i<n; i++){
            if( !valid(data[i]) ){
              std::stringstream s ;
              s << "object at index " 
                << (i+1) 
                << " ";
              if( msg ) {
                  s << msg ;
              } else {
                  s << "not compatible with requirements of " 
                    << DEMANGLE(Valid) ;
              }
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
