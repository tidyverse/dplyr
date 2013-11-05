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
            substitute(call) ;
            return call.fast_eval() ;    
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
                Result* res = get_result(call, subsets) ;
                if( res ){
                    // replace the call by the result of process
                    call = res->process(indices) ;
                    
                    // no need to go any further, we simplified the top level
                    return false ;
                }
                
                // this is where the fun really begins
                SEXP p = CDR(call);
                return replace( p ) ;
                
            }
            return false ;
        }
        
        bool replace( SEXP p ){
            SEXP obj = CAR(p) ;
            if( TYPEOF(obj) == LANGSXP ){
                Result* res = get_result(obj, subsets) ;
                if(res){
                    Shield<SEXP> value( res->process(indices) ) ;
                    SETCAR(p, value) ;
                    // SETCDR(p, R_NilValue) ;
                    return true ;
                }
                
                // try again with this calls arguments
                return replace( CDR(p) ) ;   
                
            }     
            
            if( TYPEOF(obj) == LISTSXP ){
                return replace( CDR(p) ) ;    
            }
              
            return false ;
        }
        
        Language call ;
        const DataFrame& df ;
        const SlicingIndex& indices ;
        LazyGroupedSubsets& subsets ;
    } ;
    
}
#endif
