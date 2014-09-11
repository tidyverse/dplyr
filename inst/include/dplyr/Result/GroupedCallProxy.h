#ifndef dplyr_GroupedCallProxy_H
#define dplyr_GroupedCallProxy_H

namespace dplyr {
     
    template <typename Data = GroupedDataFrame, typename Subsets = LazyGroupedSubsets>
    class GroupedCallProxy {
    public:
        typedef GroupedHybridCall<Subsets> HybridCall ;
        
        GroupedCallProxy( Call& call_, const Subsets& subsets_, const Environment& env_) : 
            call(call_), subsets(subsets_), proxies(), env(env_), hybrid(false)
        {
            set_call(call) ; 
        }
        
        GroupedCallProxy( Call& call_, const Data& data_, const Environment& env_) : 
            call(call_), subsets(data_), proxies(), env(env_), hybrid(false)
        {
            set_call(call) ; 
        }
        
        GroupedCallProxy( const Data& data_, const Environment& env_ ) : 
            subsets(data_), proxies(), env(env_), hybrid(false)
        {}
        
        ~GroupedCallProxy(){}  
        
        template <typename Container>
        SEXP get(const Container& indices){
            subsets.clear();
            if( TYPEOF(call) == LANGSXP){
                if( hybrid ) {
                    HybridCall hybrid_eval( call, indices, subsets, env ) ;
                    return hybrid_eval.eval() ;
                }
                
                int n = proxies.size() ;
                for( int i=0; i<n; i++){
                    proxies[i].set( subsets.get(proxies[i].symbol, indices ) ) ;
                }
                
                return call.eval(env) ;
            } else if( TYPEOF(call) == SYMSXP ) {
                if(subsets.count(call)){
                    return subsets.get(call, indices) ;    
                }
                return env.find( CHAR(PRINTNAME(call)) ) ;
            } else {
                // all other types that evaluate to themselves
                return call ;    
            }
        }
        
        void set_call( SEXP call_ ){
            proxies.clear() ;
            call = call_ ;
            if( TYPEOF(call) == LANGSXP ) traverse_call(call) ;
            hybrid = can_simplify_call(call) ; 
        }
        
        void input( Rcpp::String name, SEXP x ){
            subsets.input( as_symbol(name.get_sexp()) , x ) ;
        }
         
        inline int nsubsets(){
            return subsets.size() ;
        }   
        
        inline SEXP get_variable( Rcpp::String name ) const {
            return subsets.get_variable(as_symbol(name.get_sexp()) ) ;
        }
        
        inline bool is_constant() const {
            return TYPEOF(call) != LANGSXP && Rf_length(call) == 1 ;    
        }
        
        inline SEXP get_call() const {
            return call ;    
        }
        
        inline bool has_variable(SEXP symbol) const {
            return subsets.count(symbol);    
        }
        
        inline void set_env(SEXP env_){
            env = env_ ;    
        }
        
    private:
        
        inline bool can_simplify_call( SEXP call_ ){
            bool res =  can_simplify(call_);
            return res ;
        }
        
        void traverse_call( SEXP obj ){
            if( TYPEOF(obj) == LANGSXP && CAR(obj) == Rf_install("local") ) return ;
            
            if( ! Rf_isNull(obj) ){ 
                SEXP head = CAR(obj) ;
                
                switch( TYPEOF( head ) ){
                case LANGSXP: 
                    if( CAR(head) == Rf_install("function") ) break ;
                    if( CAR(head) == Rf_install("local") ) return ;
                    if( CAR(head) == Rf_install("<-") ){
                        stop( "assignments are forbidden" ) ;    
                    }
                    
                    if( Rf_length(head) == 3 ){
                        if( CAR(head) == R_DollarSymbol ){
                            SETCAR(obj, Rf_eval(head, env) ) ;
                            break ;
                        } else if( CAR(head) == Rf_install("@")) {
                            SETCAR(obj, Rf_eval(head, env) ) ;
                            break ;
                        }
                    } 
                    traverse_call( CDR(head) ) ;
                    break ;
                case LISTSXP:
                    traverse_call( head ) ;
                    traverse_call( CDR(head) ) ;
                    break ;
                   
                case SYMSXP:
                    if( TYPEOF(obj) != LANGSXP ){
                        if( ! subsets.count(head) ){
                            
                            // in the Environment -> resolve
                            try{
                                if( head == R_MissingArg ) break ;
                                if( head == Rf_install(".") ) break ;
                                
                                Shield<SEXP> x( env.find( CHAR(PRINTNAME(head)) ) ) ;
                                SETCAR( obj, x );
                            } catch(...){
                                // when the binding is not found in the environment
                                // e.g. summary(mod)$r.squared
                                // the "r.squared" is not in the env
                            }
                        } else {
                            // in the data frame
                            proxies.push_back( CallElementProxy( head, obj ) );
                        } 
                    }
                    break ;
                }
                
                traverse_call( CDR(obj) ) ;
            }    
        }
        
        Rcpp::Call call ;
        Subsets subsets ;
        std::vector<CallElementProxy> proxies ;
        Environment env; 
        bool hybrid ;
    } ;

}

#endif
