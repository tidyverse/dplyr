#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {
       
    class CallElementProxy {
    public:
        CallElementProxy(SEXP symbol_, SEXP object_) : symbol(symbol_), object(object_){}
        
        inline void set(SEXP value){ 
            SETCAR(object, value) ;
        } 
        
        SEXP symbol;
        SEXP object;
    } ;
    
    class CallProxy {
        public:
            typedef boost::unordered_map<SEXP, Subset*> SubsetMap ;
            
            CallProxy( Rcpp::Language& call_, const Rcpp::DataFrame& data_, const Environment& env_) : 
                call(call_), subset_map(), proxies(), env(env_)
            {
                init_subset_map(data_) ;
                
                // fill proxies
                traverse_call(call);  
            }
            
            CallProxy( const Rcpp::DataFrame& data_, const Environment& env_ ) : subset_map(), proxies(), env(env_) {
                init_subset_map(data_) ;
            }
            
            ~CallProxy(){
                delete_all_second( subset_map ) ;
            }  
            
            template <typename Container>
            SEXP get(const Container& indices){
                Rcpp::Shelter<SEXP> __ ;
                int n = proxies.size() ;
                boost::unordered_map<SEXP,SEXP> chunks ;
                for( int i=0; i<n; i++){
                    if( ! chunks.count( proxies[i].symbol ) )
                        chunks[ proxies[i].symbol ] = __(subset_map[proxies[i].symbol]->get(indices));
                }
                for( int i=0; i<n; i++){
                    proxies[i].set( chunks[proxies[i].symbol] ) ;     
                }
                return call.fast_eval() ;
            }
            
            void set_call( SEXP call_ ){
                proxies.clear() ;
                call = call_ ;
                traverse_call(call) ;
            }
            
            void input( Rcpp::String name, SEXP x ){
                SEXP symbol = as_symbol(name.get_sexp()) ;
                
                SubsetMap::iterator it = subset_map.find(symbol) ;
                if( it == subset_map.end() ){
                    subset_map[symbol] = subset(x) ;
                } else {
                    // found it, replacing the subset
                    delete it->second ;
                    it->second = subset(x) ;
                }
            }
             
            inline int nsubsets(){
                return subset_map.size() ;
            }   
            
            inline SEXP get_variable( Rcpp::String name ) const {
                SubsetMap::const_iterator it = subset_map.find(as_symbol(name.get_sexp())) ;
                return it->second->get( Everything() ) ;
            }
            
       private:
           
            void init_subset_map( const Rcpp::DataFrame& data ){
                // fill up subset_map
                Rcpp::CharacterVector names = data.names() ;
                int n=names.size() ;
                for( int i=0; i<n; i++){
                    subset_map[ as_symbol( names[i] ) ] = subset( data[i] ); 
                } 
            } 
            
            inline SEXP as_symbol(SEXP x) const {
                return Rf_install( CHAR(x) );
            }
           
            void traverse_call( SEXP obj ){
                 if( ! Rf_isNull(obj) ){ 
                     SEXP head = CAR(obj) ;
                     switch( TYPEOF( head ) ){
                     case LANGSXP: 
                         traverse_call( head ) ;
                         break ;
                     case SYMSXP: 
                         SubsetMap::const_iterator it = subset_map.find(head) ;
                         if( it != subset_map.end() ){
                             proxies.push_back( CallElementProxy( head, obj ) );
                         }
                         break ;
                     }
                     traverse_call( CDR(obj) ) ;
                 }    
            }
           
           Rcpp::Language call ;
           SubsetMap subset_map ;
           std::vector<CallElementProxy> proxies ;
           const Environment& env; 
    } ;

}

#endif
