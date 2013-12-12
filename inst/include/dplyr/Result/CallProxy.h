#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {
      
    class CallProxy {
    public:
        typedef dplyr_hash_map<SEXP, SEXP> DataMap ;
        
        CallProxy( const Rcpp::Language& call_, LazySubsets& subsets_, const Environment& env_) : 
            call(call_), subsets(subsets_), proxies(), env(env_)
        {
            // fill proxies
            traverse_call(call);  
            
        }
        
        CallProxy( const Rcpp::Language& call_, Rcpp::DataFrame& data_, const Environment& env_) : 
            call(call_), subsets(data_), proxies(), env(env_)
        {
            // fill proxies
            traverse_call(call);  
        }
        
        CallProxy( const Rcpp::DataFrame& data_, const Environment& env_ ) : 
            subsets(data_), proxies(), env(env_) {
        }
        
        ~CallProxy(){}  
        
        SEXP eval(){
            int n = proxies.size() ;
            for( int i=0; i<n; i++){
                proxies[i].set( subsets[proxies[i].symbol] ) ;     
            }
            Shield<SEXP> res( call.fast_eval() ) ;
            return res ;
        }
        
        void set_call( SEXP call_ ){
            proxies.clear() ;
            call = call_ ;
            traverse_call(call) ;
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
        
        void traverse_call( SEXP obj ){
            if( ! Rf_isNull(obj) ){ 
                SEXP head = CAR(obj) ;
                switch( TYPEOF( head ) ){
                case LANGSXP:
                    traverse_call( CDR(head) ) ;
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
                            // TODO: handle the case where the variable is not found in env
                            Shield<SEXP> x( env.find( CHAR(PRINTNAME(head)) ) ) ;
                            SETCAR( obj, x );
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
        
        Rcpp::Language call ;
        LazySubsets subsets ;
        std::vector<CallElementProxy> proxies ;
        Environment env; 
    } ;

}

#endif
