#ifndef dplyr_Result_MinRank_H
#define dplyr_Result_MinRank_H

namespace dplyr {
    
    template <int RTYPE>
    class MinRank : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        typedef VectorSliceVisitor<RTYPE> Slice ;
        typedef OrderVectorVisitorImpl<RTYPE,true,Slice> Visitor ;
        typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
        
        typedef dplyr_hash_map<STORAGE, std::vector<int> > Map ;
        typedef std::map<STORAGE,const std::vector<int>*> oMap ;
        
        MinRank(SEXP data_) : data(data_), map() {}
        
        virtual SEXP process( const GroupedDataFrame& gdf) {
            int ng = gdf.ngroups() ; 
            int n  = gdf.nrows() ;
            GroupedDataFrame::group_iterator git = gdf.group_begin(); 
            IntegerVector out = no_init(n) ;
            for( int i=0; i<ng; i++, ++git){
                process_slice( out, *git ) ;
            }
            return out ;
            
        }
        
        virtual SEXP process( const FullDataFrame& df ) {
            int n = df.nrows() ;
            IntegerVector out = no_init(n) ;
            process_slice(out, df.get_index() ) ;
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            int n = index.size() ;
            IntegerVector out = no_init(n) ;
            SlicingIndex fake( 0, n ) ;
            process_slice(out, fake) ;
            return out ;
        }
        
    private:
        
        void process_slice( IntegerVector& out, const SlicingIndex& index){ 
            map.clear() ;
            Slice slice(data, index) ;
            int m=index.size() ;
            for( int j=0; j<m; j++) {
                map[ slice[j] ].push_back(index[j]) ;
            }
            
            oMap ordered ;
            typename Map::const_iterator it = map.begin() ;
            for( ; it != map.end() ; ++it){
                ordered[it->first] = &it->second ;
            }
            typename oMap::const_iterator oit = ordered.begin() ;
            int j=1 ;
            for( ; oit != ordered.end(); ++oit){
                const std::vector<int>& chunk = *oit->second ;
                for( int k=0; k<chunk.size(); k++){
                    out[ chunk[k] ] = j ;
                }
                j += chunk.size() ;
            }
        }                
        
        
        SEXP data ;
        Map map ;
    } ;

}

#endif
