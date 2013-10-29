#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {
      
    class CallProxy {
    public:
        typedef boost::unordered_map<SEXP, SEXP> DataMap ;
        
        CallProxy( const Rcpp::Language& call_, const Rcpp::DataFrame& data_, const Environment& env_) : 
            call(call_), data_map(), proxies(), env(env_)
        {
            init_data_map(data_) ;
            
            // fill proxies
            traverse_call(call);  
            
        }
        
        CallProxy( const Rcpp::DataFrame& data_, const Environment& env_ ) : data_map(), proxies(), env(env_) {
            init_data_map(data_) ;
        }
        
        ~CallProxy(){}  
        
        SEXP eval(){
            int n = proxies.size() ;
            for( int i=0; i<n; i++){
                proxies[i].set( data_map[proxies[i].symbol] ) ;     
            }
            return call.fast_eval() ;
        }
        
        void set_call( SEXP call_ ){
            proxies.clear() ;
            call = call_ ;
            traverse_call(call) ;
        }
        
        void input( Rcpp::String name, SEXP x ){
            data_map[ as_symbol(name.get_sexp()) ] = x ;
        }
         
        inline int nsubsets(){
            return data_map.size() ;
        }   
        
        inline SEXP get_variable( Rcpp::String name ) const {
            DataMap::const_iterator it = data_map.find(as_symbol(name.get_sexp())) ;
            return it->second ;
        }
         
    private:
        
         void init_data_map( const Rcpp::DataFrame& data ){
             // fill up data_map
             Rcpp::CharacterVector names = data.names() ;
             int n=names.size() ;
             for( int i=0; i<n; i++){
                 data_map[ as_symbol( names[i] ) ] = data[i] ; 
             }
         } 
         
         void traverse_call( SEXP obj ){
              if( ! Rf_isNull(obj) ){ 
                  SEXP head = CAR(obj) ;
                  switch( TYPEOF( head ) ){
                  case LANGSXP: 
                      traverse_call( head ) ;
                      break ;
                  case SYMSXP: 
                      DataMap::const_iterator it = data_map.find(head) ;
                      if( it == data_map.end() ){
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
                  traverse_call( CDR(obj) ) ;
              }    
         }
        
        Rcpp::Language call ;
        DataMap data_map ;
        std::vector<CallElementProxy> proxies ;
        const Environment& env; 
    } ;

}

#endif
