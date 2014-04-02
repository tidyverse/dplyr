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
            copy_most_attributes( data, first ) ;
        }
            
        SEXP collect(){
            int ngroups = gdf.ngroups() ;
            Rcpp::Armor<SEXP> subset ;
            GroupedDataFrame::group_iterator git = gdf.group_begin() ;
            ++git ;
            for( int i=1; i<ngroups; i++, ++git){
                SlicingIndex indices = *git ;
                subset = proxy.get( indices ) ;
                grab(subset, indices); 
            }
            
            return data ;
        }
    private: 
        
        inline void grab(SEXP data, const SlicingIndex& indices){
            int n = Rf_length(data) ;
            
            if(n == indices.size() ){
                grab_along( data, indices ) ;
            } else if( n == 1) {
                grab_rep( Rcpp::internal::r_vector_start<RTYPE>(data)[0], indices ) ;    
            } else {
                std::stringstream s ;
                s << "incompatible size ("
                  << n
                  << "), expecting "
                  << indices.size()
                  << " (the group size) or 1" ;
                stop( s.str() ) ;        
            }
        }
        
        void grab_along( SEXP subset, const SlicingIndex& indices ){
            int n = indices.size();
            if( TYPEOF(subset) != RTYPE ){
                std::stringstream s ;
                s << "incompatible types, expecting a " 
                  << vector_class<RTYPE>()
                  << " vector" ;
                stop( s.str() ); 
            }
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
    class TypedGatherer : public GathererImpl<RTYPE> {
    public:
        typedef GathererImpl<RTYPE> Base ;
        
        TypedGatherer( Shield<SEXP>& first, SlicingIndex& indices, GroupedCallProxy& proxy_, const GroupedDataFrame& gdf_, const CharacterVector& classes_ ) : 
             GathererImpl<RTYPE>(first,indices,proxy_,gdf_), classes(classes_){}
             
        SEXP collect(){
            Vector<RTYPE> res( Base::collect() ) ;
            res.attr( "class" ) = classes ;
            return res ;
        }
    private:
        SEXP classes ;
        
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
    
    template <int RTYPE>
    class ConstantTypedGatherer : public ConstantGathererImpl<RTYPE> {
    public:
        ConstantTypedGatherer( Vector<RTYPE> constant, int n, SEXP classes_ ) : 
            ConstantGathererImpl<RTYPE>( constant, n), classes(classes_){}
        
        inline SEXP collect() {
            Vector<RTYPE> out = ConstantGathererImpl<RTYPE>::collect() ;
            out.attr("class") = classes ;
            return out ;
        }
        
    private:
        Vector<RTYPE> value ;
        SEXP classes ;
    } ;

    inline Gatherer* constant_gatherer(SEXP x, int n){
        switch( TYPEOF(x) ){
            case INTSXP: {
                    if( Rf_inherits(x, "Date" )) return new ConstantTypedGatherer<INTSXP>(x,n, get_date_classes() ) ;
                    return new ConstantGathererImpl<INTSXP>( x, n ) ;
            }
            case REALSXP: {
                    if( Rf_inherits(x, "POSIXct" )) return new ConstantTypedGatherer<REALSXP>(x,n, get_time_classes() ) ;
                    if( Rf_inherits(x, "Date" )) return new ConstantTypedGatherer<REALSXP>(x,n, get_date_classes() ) ;
                    return new ConstantGathererImpl<REALSXP>( x, n ) ;
            }
            case LGLSXP: return new ConstantGathererImpl<LGLSXP>( x, n ) ;
            case STRSXP: return new ConstantGathererImpl<STRSXP>( x, n ) ;
            default: break ;
        }
        return 0 ;
    }
    
    inline Gatherer* gatherer( GroupedCallProxy& proxy, const GroupedDataFrame& gdf, SEXP name ){
        GroupedDataFrame::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        Shield<SEXP> first( proxy.get(indices) ) ;
        switch( TYPEOF(first) ){
            case INTSXP:  
                {
                    if( Rf_inherits(first, "Date") ) return new TypedGatherer<INTSXP>(first, indices, proxy, gdf, get_date_classes() ) ;
                    return new GathererImpl<INTSXP> ( first, indices, proxy, gdf ) ;
                }
            case REALSXP:
                {
                    if( Rf_inherits(first, "POSIXct" ) ) return new TypedGatherer<REALSXP>(first, indices, proxy, gdf, get_time_classes() ) ;
                    if( Rf_inherits(first, "Date") ) return new TypedGatherer<REALSXP>(first, indices, proxy, gdf, get_date_classes() ) ;
                    return new GathererImpl<REALSXP>( first, indices, proxy, gdf ) ;
                }
            case LGLSXP:  return new GathererImpl<LGLSXP> ( first, indices, proxy, gdf ) ;
            case STRSXP:  return new GathererImpl<STRSXP> ( first, indices, proxy, gdf ) ;
            default: break ;
        }
        
        check_supported_type(first, name) ;
        return 0; 
    }
    
} // namespace dplyr


#endif
