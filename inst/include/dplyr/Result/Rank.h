#ifndef dplyr_Result_Rank_H
#define dplyr_Result_Rank_H

namespace dplyr {
    namespace internal {
    
        struct min_rank_increment{ 
            template <typename Container>
            inline int increment( const Container& x) const {
                return x.size() ;
            }
        } ;
    
        struct dense_rank_increment{
            template <typename Container>
            inline int increment( const Container& ) const {
                return 1 ;
            } 
        } ;
        
    }
    
    template <int RTYPE, bool ascending=true>
    class RankComparer : public comparisons<RTYPE> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        inline bool operator()(STORAGE lhs, STORAGE rhs) const {
            return comparisons<RTYPE>::is_less(lhs,rhs) ;
        }
    } ;
    
    template <int RTYPE>
    class RankComparer<RTYPE,false> : public comparisons<RTYPE> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        inline bool operator()(STORAGE lhs, STORAGE rhs) const{
            return comparisons<RTYPE>::is_greater(lhs,rhs) ;
        }
    } ;
        
    
    // powers both dense_rank and min_rank, see dplyr.cpp for how it is used
    template <int RTYPE, typename Increment, bool ascending = true>
    class Rank_Impl : public Result, public Increment {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        typedef VectorSliceVisitor<RTYPE> Slice ;
        typedef RankComparer<RTYPE,ascending> Comparer ;
        
        typedef dplyr_hash_map<STORAGE, std::vector<int> > Map ;
        typedef std::map<STORAGE,const std::vector<int>*, Comparer> oMap ;
        
        Rank_Impl(SEXP data_) : data(data_), map() {}
        
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
            return out ;
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
               
            oMap ordered;
            
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
                j += Increment::increment( chunk ) ;
            }
        }                
        
        
        SEXP data ;
        Map map ;
    } ;
    
    template <int RTYPE, bool ascending=true>
    class RowNumber : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        typedef VectorSliceVisitor<RTYPE> Slice ;
        typedef OrderVectorVisitorImpl<RTYPE,ascending,Slice> Visitor ;
        typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
            
        RowNumber(SEXP data_) : data(data_) {}
        
        virtual SEXP process( const GroupedDataFrame& gdf) {
            std::vector<int> tmp( gdf.max_group_size() ) ;
            
            int ng = gdf.ngroups() ; 
            int n  = gdf.nrows() ;
            GroupedDataFrame::group_iterator git = gdf.group_begin(); 
            IntegerVector out(n) ;
            for( int i=0; i<ng; i++, ++git){
                SlicingIndex index = *git ;
                
                // tmp <- 0:(m-1)
                int m = index.size() ;
                for( int i=0; i<m; i++) tmp[i] = i ;
                
                // order( gdf.group(i) )
                std::sort( tmp.begin(), tmp.begin() + m, 
                    Comparer( Visitor( Slice(data, index ) ) )     
                ) ;
                for( int i=0; i<m; i++) out[ index[i] ] = tmp[i] + 1 ;
            }
            return out ;
            
        }
        
        virtual SEXP process( const FullDataFrame& df ) {
            return process( df.get_index() ) ;
            
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            int nrows = index.size() ;
            IntegerVector x = seq(0, nrows -1 ) ;
            std::sort( x.begin(), x.end(), 
                Comparer( Visitor( Slice(data, index ) ) ) 
                ) ;
            IntegerVector out = no_init(nrows); 
            for( int i=0; i<nrows; i++){
                out[ x[i] ] = i + 1 ;
            }
            return out ;
        }
        
    private:
        SEXP data ;
    } ;

}

#endif
