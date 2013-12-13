#ifndef dplyr__DataDots_h
#define dplyr__DataDots_h

namespace Rcpp {
    
    class DataDots {
    public:
        
        DataDots( List calls_, List frames_ ) : 
            calls(calls_), frames(frames_), env(0), exprs(0), tags(0)
        {
            init() ;
        }
            
        SEXP eval(int i) const {
            return Rf_eval( exprs[i], env[i] );   
        }
        
        SEXP envir(int i) const {
            return env[i] ;    
        }
        
        SEXP expr( int i ) const { 
            return exprs[i] ;
        }
        
        inline size_t size() const{ 
            return exprs.size(); 
        } 
        
        
    private:
        
        void init(){
            process( frames.size() - 1, true ) ;
        }
        
        void process(int i, bool first){
            if( i < 0 ) return ;
            SEXP p = calls[i] ;
            if( TYPEOF(p) != LANGSXP ) return ;
            
            p = first ? CDDR(p) : CDR(p) ;
            SEXP head ;
            while( p != R_NilValue ){
                head = CAR(p) ;
                if( is_ellipsis(head) ) {
                    process(i-1, false) ;
                } else {
                    exprs.push_back( head ) ;
                    SEXP f = i == 0 ? R_GlobalEnv : frames[i-1] ;
                    env.push_back( f ) ;
                    tags.push_back( TAG(p) );
                }
                p = CDR(p) ;
            }
        }
        
        bool is_ellipsis( SEXP x){
             return x == R_DotsSymbol ;   
        }
        
        List calls, frames ;
        
        // all of what we put in there is already protected by R. 
        std::vector<SEXP> env ;
        std::vector<SEXP> exprs ;
        std::vector<SEXP> tags ;
        
    } ;
          
}    
#endif
