#ifndef dplyr__DataDots_h
#define dplyr__DataDots_h

namespace Rcpp {
    
    class DataDots {
    public:
        
        DataDots( Environment env ) : environments() {                
          SEXP dots = env.find( "..." );
          if( dots != R_MissingArg ){ 
            while( dots != R_NilValue ){
              Promise prom = CAR(dots) ;
              
              while(true){
                SEXP code = PRCODE(prom) ;
                if( code == R_MissingArg){
                    break ;    
                }
                if( TYPEOF(code) != PROMSXP ){
                    environments.push_back(prom.environment()) ;
                    expressions.push_back(code) ;
                    break ;  
                }
                prom = code ;
              }
              
              
              dots = CDR(dots) ;
            }
          }
        }
            
        inline const Environment& envir(int i) const {
            return environments[i] ;
        }
        
        inline SEXP expr(int i) const {
            return expressions[i] ;    
        }
        
        inline int size() const{ 
            return environments.size(); 
        } 
        
        inline bool single_env() const{
            if( environments.size() < 2 ) return true ;
            SEXP first = environments[0] ;
            for( int i=1; i<environments.size(); i++){
                if( first != environments[i] ) return false ;    
            }
            return true ;
        }
        
    private:
        std::vector<Environment> environments ;
        std::vector<SEXP> expressions;
    } ;
          
}    
#endif
