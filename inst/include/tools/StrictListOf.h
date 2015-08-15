#ifndef dplyr_tools_FilteredListOf_H
#define dplyr_tools_FilteredListOf_H

namespace Rcpp {

    template <typename T>
    struct IsValid {
        inline bool operator()(SEXP x){
            return is<T>(x) ;
        }

        inline std::string why_not(SEXP){
            stop( "not compatible with class %s", DEMANGLE(T) );
            return "";
        }
    } ;

    struct CanConvertToDataFrame {
        inline bool operator()(SEXP x){
            if( Rf_isNull(x) || is<DataFrame>(x) ) return true ;
            if( TYPEOF(x) == VECSXP && !Rf_isNull(Rf_getAttrib(x, Rf_install("names"))) ) {
                // a list that is not a data frame, so let's check if all
                // elements have the same length

                List data(x) ;
                int n = data.size() ;
                if( n == 0 ) return true ;

                int m = Rf_length(data[0]) ;
                for( int i=1; i<n; i++){
                  if( Rf_length(data[i]) != m ) return false ;
                }

                return true  ;
            }
            return false ;
        }

        inline std::string why_not(SEXP){
            return "not convertible to a data.frame" ;
        }
    } ;

    template <typename T, typename Valid = IsValid<T> >
    class StrictListOf {
    public:

        StrictListOf(SEXP data_, Valid valid = Valid() ) : data(data_){
          int n = data.size() ;
          for( int i=0; i<n; i++){
            if( !valid(data[i]) ){
              stop( "object at index %d %s", (i+1), valid.why_not(data[i]) );
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
