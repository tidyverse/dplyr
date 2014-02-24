#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {
      
    class CallProxy {
    public:
        typedef dplyr_hash_map<SEXP, SEXP> DataMap ;
        
        CallProxy( const Rcpp::Call& call_, LazySubsets& subsets_, const Environment& env_) : 
            call(call_), subsets(subsets_), proxies(), env(env_), hybrid(false)
        {
            // fill proxies
            set_call(call);  
            
        }
        
        CallProxy( const Rcpp::Call& call_, const Rcpp::DataFrame& data_, const Environment& env_) :
            call(call_), subsets(data_), proxies(), env(env_), hybrid(false)
        {
            // fill proxies
            set_call(call);  
        }
        
        CallProxy( const Rcpp::DataFrame& data_, const Environment& env_ ) : 
            subsets(data_), proxies(), env(env_), hybrid(false) {
        }
        
        ~CallProxy(){}  
        
        SEXP eval(){
            if( TYPEOF(call) == LANGSXP ){
                if(hybrid){
                    return HybridCall(call,subsets,env).eval() ;
                }
                int n = proxies.size() ;
                for( int i=0; i<n; i++){
                    proxies[i].set( subsets[proxies[i].symbol] ) ;     
                }
                Shield<SEXP> res( call.eval(env) ) ;
                return res ;
            } else if( TYPEOF(call) == SYMSXP) {
                // SYMSXP
                if( subsets.count(call) ) return subsets.get_variable(call) ;
                Shield<SEXP> res( call.eval(env) );
                return res ;
            } 
            return call ;
        }
        
        void set_call( SEXP call_ ){
            proxies.clear() ;
            call = call_ ;
            if( TYPEOF(call) == LANGSXP ) traverse_call(call) ;
            hybrid = can_simplify_call(call) ; 
        }
        
        void input( Rcpp::String name, SEXP x ){
            subsets[ as_symbol(name.get_sexp()) ] = x ;
        }
         
        inline int nsubsets(){
            return subsets.size() ;
        }   
        
        inline SEXP get_variable( Rcpp::String name ) const {
            return subsets.get_variable( Symbol(name) );
        }
        
        inline bool has_variable(SEXP symbol){
            return subsets.count(symbol) ;    
        }
         
        inline void set_env(SEXP env_){
            env = env_ ;    
        }
        
    private:
        
        inline bool can_simplify_call( SEXP call_){
            bool res =  can_simplify(call_);
            return res ;
        }
        
        void traverse_call( SEXP obj ){
            if( ! Rf_isNull(obj) ){
                SEXP head = CAR(obj) ;
                
                switch( TYPEOF( head ) ){
                case LANGSXP:
                    if( Rf_length(head) == 3 ){
                        if( CAR(head) == R_DollarSymbol ){
                            SETCAR(obj, Rf_eval(head, env) ) ;
                        } else if( CAR(head) == Rf_install("@")) {
                            SETCAR(obj, Rf_eval(head, env) ) ;
                        } else {
                          traverse_call( CDR(head) ) ;    
                        }
                    } else {
                        traverse_call( CDR(head) ) ;  
                    }
                    
                    break ;
                case LISTSXP:
                    traverse_call( head ) ;
                    traverse_call( CDR(head) ) ;
                    break ;
                case SYMSXP:
                    if( TYPEOF(obj) != LANGSXP ){
                        LazySubsets::const_iterator it = subsets.find(head) ;
                        if( it == subsets.end() ){
                            // in the Environment -> resolve
                            try{
                                Shield<SEXP> x( env.find( CHAR(PRINTNAME(head)) ) ) ;
                                SETCAR( obj, x );
                            } catch( ...){
                                // what happens when not found in environment
                            }
                        } else {
                            // in the data frame
                            proxies.push_back( CallElementProxy( head, obj ) );
                        } 
                        break ;
                    }
                }
                traverse_call( CDR(obj) ) ;
            }    
        }
        
        Rcpp::Call call ;
        LazySubsets subsets ;
        std::vector<CallElementProxy> proxies ;
        Environment env; 
        bool hybrid ;
    } ;

}

#endif
