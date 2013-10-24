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
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        GathererImpl( Rcpp::Shield<SEXP>& first, Index_0_based& indices, CallProxy& proxy_, const Rcpp::GroupedDataFrame& gdf_ ) : 
            gdf(gdf_), proxy(proxy_), data(Rcpp::no_init(gdf.nrows()))
        {
            grab( first, indices ) ;
        }
            
        SEXP collect(){
            int ngroups = gdf.ngroups() ;
            Rcpp::Armor<SEXP> subset ;
            for( int i=1; i<ngroups; i++){
                Index_0_based indices = gdf.group(i) ;
                subset = proxy.get( indices ) ;
                grab( subset, indices );
            }
            
            return data ;
        }
    private: 
        
        void grab( SEXP subset, const Index_0_based& indices ){
            int n = indices.size();
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
            for( int j=0; j<n; j++){
                data[ indices[j] ] = ptr[j] ;
            }
        }
        
        const Rcpp::GroupedDataFrame& gdf ;
        CallProxy& proxy ;
        Rcpp::Vector<RTYPE> data ;
    } ;


    inline Gatherer* gatherer( CallProxy& proxy, const GroupedDataFrame& gdf ){
        Index_0_based indices = gdf.group(0);
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
