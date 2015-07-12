#ifndef dplyr_Gatherer_H
#define dplyr_Gatherer_H

namespace dplyr {
    
    class Gatherer {
    public:
        virtual ~Gatherer(){}
        virtual SEXP collect() = 0 ;
    } ;
        
    template <int RTYPE, typename Data, typename Subsets>
    class GathererImpl : public Gatherer {
    public:
        typedef typename traits::storage_type<RTYPE>::type STORAGE ;
        typedef GroupedCallProxy<Data,Subsets> Proxy ;
        
        GathererImpl( Shield<SEXP>& first, SlicingIndex& indices, Proxy& proxy_, const Data& gdf_ ) : 
            gdf(gdf_), proxy(proxy_), data(no_init(gdf.nrows())) 
        {
            grab( first, indices ) ;
            copy_most_attributes( data, first ) ;
        }
            
        SEXP collect(){
            int ngroups = gdf.ngroups() ;
            typename Data::group_iterator git = gdf.group_begin() ;
            ++git ;
            for( int i=1; i<ngroups; i++, ++git){
                SlicingIndex indices = *git ;
                Shield<SEXP> subset( proxy.get( indices ) ) ;
                grab(subset, indices); 
            }
            return data ;
        }
        
    private: 
        
        inline void grab(SEXP data, const SlicingIndex& indices){
            int n = Rf_length(data) ;
            
            check_type(data) ;
            if(n == indices.size() ){
                grab_along( data, indices ) ;
            } else if( n == 1) {
                grab_rep( Rcpp::internal::r_vector_start<RTYPE>(data)[0], indices ) ;    
            } else {
                stop ( "incompatible size (%d), expecting %d (the group size) or 1",
                        n, indices.size()) ;
            }
        }
        
        void grab_along( SEXP subset, const SlicingIndex& indices ){
            int n = indices.size();
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
            for( int j=0; j<n; j++){
                data[ indices[j] ] = ptr[j] ;
            }
        }
        
        void check_type(SEXP subset){
            if( TYPEOF(subset) != RTYPE ){
                stop( "incompatible types, expecting a %s vector", vector_class<RTYPE>() ) ;
            }
        }
        
        void grab_rep( STORAGE value, const SlicingIndex& indices ){
            int n = indices.size();
            for( int j=0; j<n; j++){
                data[ indices[j] ] = value ;
            }
        }
        
        const Data& gdf ;
        Proxy& proxy ;
        Vector<RTYPE> data ;
        
    } ;
    
    template <int RTYPE, typename Data, typename Subsets>
    class ConstantGathererImpl : public Gatherer {
    public:
        ConstantGathererImpl( Vector<RTYPE> constant, int n ) : 
            value( n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0] )
        {
            copy_most_attributes( value, constant ) ;    
        }
        
        inline SEXP collect() {
            return value ;
        }
        
    private:
        Vector<RTYPE> value ;
    } ;
    
    template <typename Data, typename Subsets>
    inline Gatherer* constant_gatherer(SEXP x, int n){
        if( Rf_inherits(x, "POSIXlt" ) ){
            stop("`mutate` does not support `POSIXlt` results");    
        }
        switch( TYPEOF(x) ){
            case INTSXP:  return new ConstantGathererImpl<INTSXP,Data,Subsets>( x, n ) ;
            case REALSXP: return new ConstantGathererImpl<REALSXP,Data,Subsets>( x, n ) ;
            case LGLSXP:  return new ConstantGathererImpl<LGLSXP,Data,Subsets>( x, n ) ;
            case STRSXP:  return new ConstantGathererImpl<STRSXP,Data,Subsets>( x, n ) ;
            case CPLXSXP: return new ConstantGathererImpl<CPLXSXP,Data,Subsets>( x, n ) ;
            case VECSXP:  return new ConstantGathererImpl<STRSXP,Data,Subsets>( x, n ) ;
            default: break ;
        }
        return 0 ;
    }
    
    template <typename Data, typename Subsets>
    inline Gatherer* gatherer( GroupedCallProxy<Data,Subsets>& proxy, const Data& gdf, SEXP name ){
        typename Data::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        Shield<SEXP> first( proxy.get(indices) ) ;
        if( Rf_inherits(first, "POSIXlt" ) ){
            stop("`mutate` does not support `POSIXlt` results");    
        }
        switch( TYPEOF(first) ){
            case INTSXP:  return new GathererImpl<INTSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case REALSXP: return new GathererImpl<REALSXP,Data,Subsets>( first, indices, proxy, gdf ) ;
            case LGLSXP:  return new GathererImpl<LGLSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case STRSXP:  return new GathererImpl<STRSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case VECSXP:  return new GathererImpl<VECSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case CPLXSXP: return new GathererImpl<CPLXSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            default: break ;
        }
        check_supported_type(first, name) ;
        return 0; 
    }
    
} // namespace dplyr


#endif
