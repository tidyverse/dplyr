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
                grab( subset, indices );
            }
            
            return data ;
        }
    private: 
        
        void grab( SEXP subset, const SlicingIndex& indices ){
            int n = indices.size();
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
            for( int j=0; j<n; j++){
                // TODO: take advantage of SlicingIndex
                data[ indices[j] ] = ptr[j] ;
            }
        }
        
        const GroupedDataFrame& gdf ;
        GroupedCallProxy& proxy ;
        Vector<RTYPE> data ;
        
    } ;


    inline Gatherer* gatherer( GroupedCallProxy& proxy, const GroupedDataFrame& gdf ){
        GroupedDataFrame::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        Shield<SEXP> first( proxy.get(indices) ) ;
        switch( TYPEOF(first) ){
            // TODO: perhaps pass git down to GathererImpl
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
