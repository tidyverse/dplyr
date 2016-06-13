#ifndef dplyr__DotsOf_h
#define dplyr__DotsOf_h

namespace Rcpp {

    template <typename T>
    class DotsOf {
    public:

        DotsOf( Environment env ) : data() {
          SEXP dots = env.find( "..." );
          if( dots != R_MissingArg ) collect(dots) ;
        }

        inline T& operator[](int i){
            return data[i] ;
        }

        inline int size() const{
            return data.size();
        }

    private:
        std::vector<T> data ;

        void collect( SEXP dots){
            int np = 0 ;

            while( dots != R_NilValue ){
              SEXP prom = CAR(dots) ;

              while(true){
                SEXP code = PRCODE(prom) ;
                if( TYPEOF(code) != PROMSXP ){
                  break ;
                }
                prom = code ;
              }
              SEXP x = PRVALUE(prom) ;
              if( x == R_UnboundValue ){
                x = PROTECT(Rf_eval(PRCODE(prom), PRENV(prom))) ;
                np++ ;
              }
              if( is<T>(x) ){
                data.push_back( as<T>(x) ) ;
              }
              dots = CDR(dots) ;
            }
            if(np) UNPROTECT(np) ;
        }

    } ;

    class Dots {
    public:

        Dots( Environment env ) : data() {
          SEXP dots = env.find( "..." );
          if( dots != R_MissingArg ) collect(dots) ;
        }

        inline SEXP operator[](int i){
            return data[i] ;
        }

        inline int size() const{
            return data.size();
        }

        inline int names() const{
            return data.names();
        }

    private:
        List data ;

        void collect( SEXP dots){
            int np = 0 ;

            while( dots != R_NilValue ){
              SEXP prom = CAR(dots) ;

              while(true){
                SEXP code = PRCODE(prom) ;
                if( TYPEOF(code) != PROMSXP ){
                  break ;
                }
                prom = code ;
              }
              SEXP x = PRVALUE(prom) ;
              if( x == R_UnboundValue ){
                x = PROTECT(Rf_eval(PRCODE(prom), PRENV(prom))) ;
                np++ ;
              }
              data.push_back(x) ;
              dots = CDR(dots) ;
            }
            if(np) UNPROTECT(np) ;
        }

    } ;


}
#endif
