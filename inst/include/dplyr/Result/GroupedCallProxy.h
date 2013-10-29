#ifndef dplyr_GroupedCallProxy_H
#define dplyr_GroupedCallProxy_H

namespace dplyr {
     
    class LazyGroupedSubsets {
    public:
        typedef boost::unordered_map<SEXP, GroupedSubset*> GroupedSubsetMap ;
        typedef boost::unordered_map<SEXP, SEXP> ResolvedSubsetMap ;
        
        LazyGroupedSubsets( const GroupedDataFrame& gdf_ ): gdf(gdf_), subset_map(), resolved_map() {
            int max_size = gdf.max_group_size() ;
            const DataFrame& data = gdf.data() ;
            CharacterVector names = data.names() ;
            int n = data.size() ;
            for( int i=0; i<n; i++){
                subset_map[ as_symbol( names[i] ) ] = grouped_subset( data[i], max_size );    
            }
        }
        
        void clear(){
            resolved_map.clear() ;
        }
        
        int count( SEXP head) const {
            return subset_map.count(head);    
        }
        
        int size() const {
            return subset_map.size();
        }
        
        SEXP get_variable( SEXP symbol ) const {
            GroupedSubsetMap::const_iterator it = subset_map.find( symbol );
            return it->second->get_variable() ;  
        }
        SEXP get( SEXP symbol, const SlicingIndex& indices ){
            ResolvedSubsetMap::const_iterator it = resolved_map.find( symbol ) ;
            if( it == resolved_map.end() ){
                SEXP res = subset_map[symbol]->get( indices ) ;
                resolved_map[symbol] = res ;
                return res ;
            } else {
                return it->second ;    
            }
        }
        
        ~LazyGroupedSubsets(){
            delete_all_second( subset_map ) ;    
        }
        
        void input(SEXP symbol, SEXP x){                    
            GroupedSubset* sub = grouped_subset(x, gdf.max_group_size() ) ;
            
            GroupedSubsetMap::iterator it = subset_map.find(symbol) ;
            if( it == subset_map.end() ){
                subset_map[symbol] = sub ;
            } else {
                // found it, replacing the subset
                delete it->second ;
                it->second = sub ;
            }
        }

    private:
        const GroupedDataFrame& gdf ;
        GroupedSubsetMap subset_map ;
        ResolvedSubsetMap resolved_map ;
    } ;
    
    class GroupedCallProxy {
    public:
        typedef boost::unordered_map<SEXP, Subset*> SubsetMap ;
        
        GroupedCallProxy( Language& call_, const GroupedDataFrame& data_, const Environment& env_) : 
            call(call_), subsets(data_), proxies(), env(env_), data(data_)
        {
            // fill proxies
            traverse_call(call);  
        }
        
        GroupedCallProxy( const GroupedDataFrame& data_, const Environment& env_ ) : 
            subsets(data_), proxies(), env(env_), data(data_) {}
        
        ~GroupedCallProxy(){}  
        
        template <typename Container>
        SEXP get(const Container& indices){
            subsets.clear();
            int n = proxies.size() ;
            for( int i=0; i<n; i++){
                proxies[i].set( subsets.get(proxies[i].symbol, indices ) ) ;  
            }
            return call.fast_eval() ;
        }
        
        void set_call( SEXP call_ ){
            proxies.clear() ;
            call = call_ ;
            traverse_call(call) ;
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
        
    private:
        
         void traverse_call( SEXP obj ){
              if( ! Rf_isNull(obj) ){ 
                  SEXP head = CAR(obj) ;
                  switch( TYPEOF( head ) ){
                  case LANGSXP: 
                      traverse_call( head ) ;
                      break ;
                  case SYMSXP: 
                      if( subsets.count(head) ){
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
        LazyGroupedSubsets subsets ;
        std::vector<CallElementProxy> proxies ;
        const Environment& env; 
        const GroupedDataFrame& data ; 
    } ;

}

#endif
