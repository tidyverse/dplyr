#ifndef dplyr__DataDots_h
#define dplyr__DataDots_h

namespace Rcpp {
    
    class DataDots {
    public:
        
        DataDots( Environment env ) : environments(), parent(env) {                
          SEXP dots = env.find( "..." );
          if( dots != R_MissingArg ){
            
            int i= 0 ;
            while( dots != R_NilValue ){
              Promise prom = CAR(dots) ;
              
              while(true){                                                  
                SEXP code = PRCODE(prom) ;
                if( code == R_MissingArg){
                    break ;    
                }
                if( TYPEOF(code) != PROMSXP ){
                    environments.push_back(prom.environment()) ;
                    index.push_back(i) ;
                    break ;  
                }
                prom = code ;
              }
              
              dots = CDR(dots) ; 
              i++ ;
            }
          }
        }
            
        inline const Environment& envir(int i) const {
            return environments[i] ;
        }
        
        inline int expr_index(int i) const {
            return index[i] ; 
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
        
        inline const Environment& parent_env() const {
            return parent ;
        }
        
    private:
        std::vector<Environment> environments ;
        std::vector<int> index;
        Environment parent ;
    } ;
          
}    
#endif
