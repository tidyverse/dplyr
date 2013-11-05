#ifndef dplyr_GroupedCallProxy_H
#define dplyr_GroupedCallProxy_H

namespace dplyr {
     
    class GroupedHybridCall {
    public:
        GroupedHybridCall( const Language& call_, const DataFrame& df_, const SlicingIndex& indices_, LazyGroupedSubsets& subsets_ ) : 
            call( clone(call_) ), df( df_ ), indices(indices_) // , subsets(subsets_) 
        {
            while( simplified(call, call) ) ;
        }
        
        SEXP eval(){
            return call.fast_eval() ;    
        }
        
    private:
        
        bool simplified( SEXP obj, SEXP parent ){
            if( TYPEOF(obj) == LISTSXP ){
                bool res = simplified( CAR(obj), parent ) ;
                if( res ) return true ;
                return simplified( CDR(obj), CDR(obj) );
            }
            
            if( TYPEOF(obj) == LANGSXP ) {
                Result* res = get_result( obj, df ) ;
                if( res ){
                    Shield<SEXP> value( res->process(indices) ) ;
                    SETCAR( parent, value ) ;
                    SETCDR( parent, R_NilValue );
                    return true ;
                }
                return simplified( CDR(obj), CDR(obj) );
            }
            return false ;
        }
        
        Language call ;
        const DataFrame& df ;
        const SlicingIndex& indices ;
        // LazyGroupedSubsets& subsets ;
    } ;
    
}
#endif
