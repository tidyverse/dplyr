#ifndef dplyr_Result_Rank_H
#define dplyr_Result_Rank_H

namespace dplyr {
    namespace internal {
    
        struct min_rank_increment{ 
            typedef IntegerVector OutputVector ;
            typedef int scalar_type ;
            
            template <typename Container>
            inline int post_increment( const Container& x, int) const {
                return x.size() ;
            }
            
            template <typename Container>
            inline int pre_increment( const Container& x, int) const {
                return 0 ;
            }
            
            inline int start() const {
                return 1 ;    
            }
        } ;
    
        struct dense_rank_increment{
            typedef IntegerVector OutputVector ;
            typedef int scalar_type ;
            
            template <typename Container>
            inline int post_increment( const Container&, int) const {
                return 1 ;
            } 
            
            template <typename Container>
            inline int pre_increment( const Container&, int) const {
                return 0 ;
            } 
            
            inline int start() const {
                return 1 ;    
            }
        } ;
        
        struct percent_rank_increment{
            typedef NumericVector OutputVector ;
            typedef double scalar_type ;
            
            template <typename Container>
            inline double post_increment( const Container& x, int m) const {
                return (double)x.size() / ( m - 1 ) ;
            } 
            
            template <typename Container>
            inline double pre_increment( const Container& x, int m) const {
                return 0.0 ;
            } 
            
            inline double start() const {
                return 0.0 ;    
            }
        } ;
        
        struct cume_dist_increment{
            typedef NumericVector OutputVector ;
            typedef double scalar_type ;
            
            template <typename Container>
            inline double post_increment( const Container& x, int m) const {
                return 0.0 ;
            } 
            
            template <typename Container>
            inline double pre_increment( const Container& x, int m) const {
                return (double)x.size() / m ;
            } 
            
            inline double start() const {
                return 0.0 ;    
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
        typedef typename Increment::OutputVector OutputVector ;
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
            OutputVector out = no_init(n) ;
            for( int i=0; i<ng; i++, ++git){
                process_slice( out, *git ) ;
            }
            return out ;
            
        }
        
        virtual SEXP process( const FullDataFrame& df ) {
            int n = df.nrows() ;
            OutputVector out = no_init(n) ;
            process_slice(out, df.get_index() ) ;
            return out ;
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            int n = index.size() ;
            OutputVector out = no_init(n) ;
            process_slice(out, index) ;
            return out ;
        }
        
    private:
        
        void process_slice( OutputVector& out, const SlicingIndex& index){ 
            map.clear() ;
            Slice slice(data, index) ;
            int m=index.size() ;
            for( int j=0; j<m; j++) {
                map[ slice[j] ].push_back(j) ;
            }
               
            oMap ordered;
            
            typename Map::const_iterator it = map.begin() ;
            for( ; it != map.end() ; ++it){
                ordered[it->first] = &it->second ;
            }
            typename oMap::const_iterator oit = ordered.begin() ;
            typename Increment::scalar_type j = Increment::start() ;
            for( ; oit != ordered.end(); ++oit){
                const std::vector<int>& chunk = *oit->second ;
                j += Increment::pre_increment( chunk, m ) ;
                for( int k=0; k<chunk.size(); k++){
                    out[ chunk[k] ] = j ;
                }
                j += Increment::post_increment( chunk, m ) ;
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
                for( int j=0; j<m; j++) tmp[j] = j ;
                
                // order( gdf.group(i) )
                std::sort( tmp.begin(), tmp.begin() + m, 
                    Comparer( Visitor( Slice(data, index ) ) )     
                ) ;
                for( int j=0; j<m; j++) out[ index[j] ] = tmp[j] + 1 ;
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
    
    template <int RTYPE, bool ascending=true>
    class Ntile : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        typedef VectorSliceVisitor<RTYPE> Slice ;
        typedef OrderVectorVisitorImpl<RTYPE,ascending,Slice> Visitor ;
        typedef Compare_Single_OrderVisitor<Visitor> Comparer ;
            
        Ntile(SEXP data_, double ntiles_ ) : data(data_), ntiles(ntiles_) {}
        
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
                for( int j=0; j<m; j++) tmp[j] = j ;
                
                // order( gdf.group(i) )
                std::sort( tmp.begin(), tmp.begin() + m, 
                    Comparer( Visitor( Slice(data, index ) ) )     
                ) ;
                for( int j=0; j<m; j++) out[ index[j] ] = (int)floor( (ntiles * tmp[j]) / m ) + 1;
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
                out[ x[i] ] = (int)floor(ntiles * i / nrows ) + 1;
            }
            return out ;
        }
        
    private:
        SEXP data ;
        double ntiles ;
    } ;
    
    class RowNumber_0 : public Result {
    public:
        
        virtual SEXP process( const GroupedDataFrame& gdf ){
            int n = gdf.nrows(), ng = gdf.ngroups() ;
            
            IntegerVector res = no_init(n) ;
            GroupedDataFrame::group_iterator git = gdf.group_begin() ;
            for( int i=0; i<ng; i++, ++git){
                SlicingIndex index = *git ;
                int m = index.size() ;
                for( int j=0; j<m; j++) res[index[j]] = j + 1 ;
            }
            return res ;
        }
        
        virtual SEXP process( const FullDataFrame& df ) {
            IntegerVector res = seq(1, df.nrows() ) ;
            return res ;
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            IntegerVector res = seq(1, index.size() ) ;
            return res ;
        }
        
    } ;
    
}

#endif
