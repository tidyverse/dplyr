#ifndef dplyr_HybridCall_H
#define dplyr_HybridCall_H

namespace dplyr {
     
    class HybridCall {                                                                                    
    public:
        HybridCall( const Call& call_, LazySubsets& subsets_, const Environment& env_ ) : 
            call( clone(call_) ), subsets(subsets_), env(env_), indices(0,subsets.nrows())
        {
            while( simplified() ) ;
        }
        
        SEXP eval(){
            if( TYPEOF(call) == LANGSXP ){
                substitute(call) ;
                return Rf_eval( call, env ) ;
            } else if(TYPEOF(call) == SYMSXP) {
                if(subsets.count(call)){
                    return subsets.get_variable(call) ;    
                }
                return env.find( CHAR(PRINTNAME(call)) ) ;    
            }
            return call ;
        }
        
    private:
        
        void substitute( SEXP obj){
            if( ! Rf_isNull(obj) ){ 
                 SEXP head = CAR(obj) ;
                 switch( TYPEOF( head ) ){
                 case LISTSXP:
                 case LANGSXP: 
                     substitute( CDR(head) ) ;
                     break ;
                 case SYMSXP:
                    if( TYPEOF(obj) != LANGSXP ){
                       if( subsets.count(head) ){
                           SETCAR(obj, subsets.get_variable(head) ) ;
                       } 
                    }
                    break ;
                 }
                 substitute( CDR(obj) ) ;
             }        
        }
        
        bool simplified(){
            // initial
            if( TYPEOF(call) == LANGSXP ){
                Result* res = get_handler(call, subsets, env) ;
                
                if( res ){
                    // replace the call by the result of process
                    call = res->process(indices) ;
                    
                    // no need to go any further, we simplified the top level
                    return true ;
                }
                
                return replace( CDR(call)) ;
                
            }
            return false ;
        }
        
        bool replace( SEXP p ){
            
            SEXP obj = CAR(p) ;
            
            if( TYPEOF(obj) == LANGSXP ){
                Result* res = get_handler(obj, subsets, env) ;
                if(res){
                    SETCAR(p, res->process(indices) ) ;
                    return true ;
                }
                
                if( replace( CDR(obj) ) ) return true ;   
            }     
            
            if( TYPEOF(p) == LISTSXP ){
                return replace( CDR(p) ) ;    
            }
              
            return false ;
        }
        
        Armor<SEXP> call ;
        LazySubsets& subsets ;
        const Environment& env ;
        SlicingIndex indices ;
    } ;
    
}
#endif
