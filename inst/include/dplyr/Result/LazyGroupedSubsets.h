#ifndef dplyr_LazyGroupedSubsets_H
#define dplyr_LazyGroupedSubsets_H

namespace dplyr {
        
    class LazyGroupedSubsets : public LazySubsets {
    public:
        typedef dplyr_hash_map<SEXP, GroupedSubset*> GroupedSubsetMap ;
        typedef dplyr_hash_map<SEXP, SEXP> ResolvedSubsetMap ;
        
        LazyGroupedSubsets( const GroupedDataFrame& gdf_ ): gdf(gdf_), subset_map(), resolved_map(), owner(true) {
            int max_size = gdf.max_group_size() ;
            const DataFrame& data = gdf.data() ;
            CharacterVector names = data.names() ;
            int n = data.size() ;
            for( int i=0; i<n; i++){
                subset_map[ as_symbol( names[i] ) ] = grouped_subset( data[i], max_size );    
            }
        }
        
        LazyGroupedSubsets( const LazyGroupedSubsets& other) : 
            gdf(other.gdf), subset_map(other.subset_map), resolved_map(other.resolved_map), owner(false)
        {}
        
        void clear(){
            resolved_map.clear() ;
        }
        
        int count(SEXP head) const {
            return subset_map.count(head);    
        }
        
        virtual int size() const {
            return subset_map.size();
        }
        
        SEXP get_variable( SEXP symbol ) const {
            GroupedSubsetMap::const_iterator it = subset_map.find( symbol );
            if( it == subset_map.end() ){
                std::stringstream s ;
                s << "variable '" 
                  << CHAR(PRINTNAME(symbol))
                  << "' not found in the dataset" ;
                stop(s.str()) ;
            }
            return it->second->get_variable() ;  
        }
        bool is_summary( SEXP symbol ) const {
            GroupedSubsetMap::const_iterator it = subset_map.find( symbol );
            return it->second->is_summary() ;    
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
            if(owner) delete_all_second( subset_map ) ;    
        }
        
        void input(SEXP symbol, SEXP x){                    
            input_subset( symbol, grouped_subset(x, gdf.max_group_size() ) );
        }
        
        void input(SEXP symbol, SummarisedVariable x){                    
            input_subset( symbol, summarised_grouped_subset(x, gdf.max_group_size() ) ) ;
        }

    private:
        const GroupedDataFrame& gdf ;
        GroupedSubsetMap subset_map ;
        ResolvedSubsetMap resolved_map ;
        bool owner ; 
        
        void input_subset(SEXP symbol, GroupedSubset* sub){
            GroupedSubsetMap::iterator it = subset_map.find(symbol) ;
            if( it == subset_map.end() ){
                subset_map[symbol] = sub ;
            } else {
                // found it, replacing the subset
                delete it->second ;
                it->second = sub ;
            }
        }
    } ;

}
#endif
