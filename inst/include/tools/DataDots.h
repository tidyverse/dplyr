#ifndef dplyr__DataDots_h
#define dplyr__DataDots_h

namespace Rcpp {
    
    class DataDots {
    public:
        
        DataDots( List args, Environment env ) : environments() {                
          SEXP dots = env.find( "..." );
          
          while( dots != R_NilValue ){
            Promise prom = CAR(dots) ;
            
            while(true){
              SEXP code = PRCODE(prom) ;
              if( TYPEOF(code) != PROMSXP ){
                break ;  
              }
              prom = code ;
            }
            environments.push_back(prom.environment()) ;
            
            dots = CDR(dots) ;
          }
        }
            
        inline SEXP envir(int i) const {
            return environments[i] ;    
        }
        
        inline int size() const{ 
            return environments.size(); 
        } 
        
        
    private:
        
        std::vector<SEXP> environments ;
    } ;
          
}    
#endif
