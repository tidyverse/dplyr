#ifndef dplyr_GroupedHybridCall_H
#define dplyr_GroupedHybridCall_H

namespace dplyr {
     
    class GroupedHybridCall {
    public:
        GroupedHybridCall( const Language& call_, const DataFrame& df_, const SlicingIndex& indices_, LazyGroupedSubsets& subsets_ ) : 
            call( clone(call_) ), df( df_ ), indices(indices_), subsets(subsets_) 
        {
            while( simplified() ) ;
        }
        
        SEXP eval(){
            if( TYPEOF(call) == LANGSXP ){
                substitute(call) ;
                return Rf_eval( call, R_GlobalEnv ) ;
            } else {
                return call ;    
            }
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
                           SETCAR(obj, subsets.get(head, indices) ) ;
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
                Result* res = get_handler(call, subsets) ;
                if( res ){
                    // replace the call by the result of process
                    Shield<SEXP> out( res->process(indices) );
                    
                    #if RCPP_VERSION < Rcpp_Version(0,10,7)
                    call = (SEXP)out ;
                    #else
                    call = out ;
                    #endif
                    
                    // no need to go any further, we simplified the top level
                    return true ;
                }
                
                return replace( CDR(call) ) ;
                
            }
            return false ;
        }
        
        bool replace( SEXP p ){
            SEXP obj = CAR(p) ;
            if( TYPEOF(obj) == LANGSXP ){
                Result* res = get_handler(obj, subsets) ;
                if(res){
                    SETCAR(p, res->process(indices) ) ;
                    return true ;
                }
                
                return replace( CDR(p) ) ;   
            }     
            
            if( TYPEOF(p) == LISTSXP ){
                return replace( CDR(p) ) ;    
            }
              
            return false ;
        }
        
        Armor<SEXP> call ;
        const DataFrame& df ;
        const SlicingIndex& indices ;
        LazyGroupedSubsets& subsets ;
    } ;
    
}
#endif
