#ifndef dplyr_Gatherer_H
#define dplyr_Gatherer_H

namespace dplyr {
    
    class Gatherer {
    public:
        virtual ~Gatherer(){}
        virtual SEXP collect() = 0 ;
    } ;
        
    template <int RTYPE>
    class GathererImpl : public Gatherer {
    public:
        typedef typename traits::storage_type<RTYPE>::type STORAGE ;
        
        GathererImpl( Shield<SEXP>& first, SlicingIndex& indices, GroupedCallProxy& proxy_, const GroupedDataFrame& gdf_ ) : 
            gdf(gdf_), proxy(proxy_), data(no_init(gdf.nrows())) 
        {
            grab( first, indices ) ;
        }
            
        SEXP collect(){
            int ngroups = gdf.ngroups() ;
            Rcpp::Armor<SEXP> subset ;
            GroupedDataFrame::group_iterator git = gdf.group_begin() ;
            ++git ;
            for( int i=1; i<ngroups; i++, ++git){
                SlicingIndex indices = *git ;
                subset = proxy.get( indices ) ;
                int n_subset = Rf_length(subset) ;
                if( n_subset == indices.size() ){
                    grab( subset, indices );
                } else if( n_subset == 1 ){
                    grab_rep( Rcpp::internal::r_vector_start<RTYPE>(subset)[0], indices );
                } else {
                    std::stringstream s ;
                    s << "incompatible size ("
                      << n_subset
                      << "), expecting "
                      << indices.size()
                      << " (the group size) or 1" ;
                    stop( s.str() ) ;    
                }
            }
            
            return data ;
        }
    private: 
        
        void grab( SEXP subset, const SlicingIndex& indices ){
            int n = indices.size();
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
            for( int j=0; j<n; j++){
                data[ indices[j] ] = ptr[j] ;
            }
        }
        
        void grab_rep( STORAGE value, const SlicingIndex& indices ){
            int n = indices.size();
            for( int j=0; j<n; j++){
                data[ indices[j] ] = value ;
            }
        }
        
        const GroupedDataFrame& gdf ;
        GroupedCallProxy& proxy ;
        Vector<RTYPE> data ;
        
    } ;
    
    template <int RTYPE>
    class ConstantGathererImpl : public Gatherer {
    public:
        ConstantGathererImpl( Vector<RTYPE> constant, int n ) : value( n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0] ){}
        
        inline SEXP collect() {
            return value ;
        }
        
    private:
        Vector<RTYPE> value ;
    } ;

    inline Gatherer* constant_gatherer(SEXP x, int n){
        switch( TYPEOF(x) ){
            case INTSXP: return new ConstantGathererImpl<INTSXP>( x, n ) ;
            case REALSXP: return new ConstantGathererImpl<REALSXP>( x, n ) ;
            case LGLSXP: return new ConstantGathererImpl<LGLSXP>( x, n ) ;
            case STRSXP: return new ConstantGathererImpl<STRSXP>( x, n ) ;
            default: break ;
        }
        return 0 ;
    }
    
    inline Gatherer* gatherer( GroupedCallProxy& proxy, const GroupedDataFrame& gdf ){
        GroupedDataFrame::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        Shield<SEXP> first( proxy.get(indices) ) ;
        switch( TYPEOF(first) ){
            case INTSXP:  return new GathererImpl<INTSXP> ( first, indices, proxy, gdf ) ;
            case REALSXP: return new GathererImpl<REALSXP>( first, indices, proxy, gdf ) ;
            case LGLSXP:  return new GathererImpl<LGLSXP> ( first, indices, proxy, gdf ) ;
            case STRSXP:  return new GathererImpl<STRSXP> ( first, indices, proxy, gdf ) ;
            default: break ;
        }
        // should not happen, but if it does, we should handle it
        return 0; 
    }
    
} // namespace dplyr


#endif
