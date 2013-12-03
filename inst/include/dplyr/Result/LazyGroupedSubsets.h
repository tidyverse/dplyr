#ifndef dplyr_LazyGroupedSubsets_H
#define dplyr_LazyGroupedSubsets_H

namespace dplyr {
     
    class LazySubsets {
    public:
        typedef dplyr_hash_map<SEXP,SEXP> DataMap ;
        typedef DataMap::const_iterator const_iterator ;
        
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
        
        virtual void input( SEXP symbol, SEXP x){
            data_map[symbol] = x;    
        }
        
        inline const_iterator find(SEXP x) const {
            return data_map.find(x) ;
        }
        
        inline const_iterator end() const {
            return data_map.end() ;   
        }
        
        inline int size() const{ 
            return data_map.size() ; 
        }
        
        inline SEXP& operator[](SEXP symbol){
            return data_map[symbol] ;    
        }
        
    private:
        DataMap data_map ;
    } ;
    
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
