#ifndef dplyr_LazyGroupedSubsets_H
#define dplyr_LazyGroupedSubsets_H

namespace dplyr {
     
    class LazySubsets {
    public:
        typedef dplyr_hash_map<SEXP,SEXP> DataMap ;
        LazySubsets(){}
        
        LazySubsets( const DataFrame& df) : data_map(){
            CharacterVector names = df.names() ;
            for( int i=0; i<df.size(); i++){
                data_map[as_symbol(names[i])] = df[i] ;    
            }
        }
        virtual ~LazySubsets(){}
        virtual SEXP get_variable(SEXP symbol) const {
            DataMap::const_iterator it = data_map.find(symbol) ;
            return it->second ;
        }
        virtual int count(SEXP symbol) const{
            return data_map.count(symbol);    
        }
    private:
        DataMap data_map ;
    } ;
    
    class LazyGroupedSubsets : public LazySubsets {
    public:
        typedef dplyr_hash_map<SEXP, GroupedSubset*> GroupedSubsetMap ;
        typedef dplyr_hash_map<SEXP, SEXP> ResolvedSubsetMap ;
        
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

}
#endif
