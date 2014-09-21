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
            Rcpp::Armor<SEXP> subset ;
            typename Data::group_iterator git = gdf.group_begin() ;
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
            
            check_type(data) ;
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
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>( subset ) ;
            for( int j=0; j<n; j++){
                data[ indices[j] ] = ptr[j] ;
            }
        }
        
        void check_type(SEXP subset){
            if( TYPEOF(subset) != RTYPE ){
                std::stringstream s ;
                s << "incompatible types, expecting a " 
                  << vector_class<RTYPE>()
                  << " vector" ;
                stop( s.str() ); 
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
    class TypedGatherer : public GathererImpl<RTYPE,Data,Subsets> {
    public:
        typedef GathererImpl<RTYPE,Data,Subsets> Base ;
        
        TypedGatherer( Shield<SEXP>& first, SlicingIndex& indices, GroupedCallProxy<Data,Subsets>& proxy_, const Data& gdf_, const CharacterVector& classes_ ) : 
             GathererImpl<RTYPE,Data,Subsets>(first,indices,proxy_,gdf_), classes(classes_){}
             
        SEXP collect(){
            Vector<RTYPE> res( Base::collect() ) ;
            res.attr( "class" ) = classes ;
            return res ;
        }
    private:
        SEXP classes ;
        
    } ;
    
    template <int RTYPE, typename Data, typename Subsets>
    class ConstantGathererImpl : public Gatherer {
    public:
        ConstantGathererImpl( Vector<RTYPE> constant, int n ) : value( n, Rcpp::internal::r_vector_start<RTYPE>(constant)[0] ){}
        
        inline SEXP collect() {
            return value ;
        }
        
    private:
        Vector<RTYPE> value ;
    } ;
    
    template <int RTYPE, typename Data, typename Subsets>
    class ConstantTypedGatherer : public ConstantGathererImpl<RTYPE, Data, Subsets> {
    public:
        ConstantTypedGatherer( Vector<RTYPE> constant, int n, SEXP classes_ ) : 
            ConstantGathererImpl<RTYPE,Data,Subsets>( constant, n), classes(classes_){}
        
        inline SEXP collect() {
            Vector<RTYPE> out = ConstantGathererImpl<RTYPE,Data,Subsets>::collect() ;
            out.attr("class") = classes ;
            return out ;
        }
        
    private:
        Vector<RTYPE> value ;
        SEXP classes ;
    } ;
    
    template <int RTYPE, typename Data, typename Subsets>
    class ConstantDifftimeGatherer : public ConstantGathererImpl<RTYPE, Data, Subsets> {
    public:
        ConstantDifftimeGatherer( Vector<RTYPE> constant, int n ) : 
            ConstantGathererImpl<RTYPE,Data,Subsets>( constant, n), 
            units(constant.attr("units"))
        {}
        
        inline SEXP collect() {
            Vector<RTYPE> out = ConstantGathererImpl<RTYPE,Data,Subsets>::collect() ;
            out.attr("class") = "difftime" ;
            out.attr("units") = units ; 
            return out ;
        }
        
    private:
        Vector<RTYPE> value ;
        CharacterVector units ;
    } ;

    template <typename Data, typename Subsets>
    inline Gatherer* constant_gatherer(SEXP x, int n){
        switch( TYPEOF(x) ){
            case INTSXP: {
                    if( Rf_inherits(x, "Date" )) return new ConstantTypedGatherer<INTSXP,Data,Subsets>(x,n, get_date_classes() ) ;
                    return new ConstantGathererImpl<INTSXP,Data,Subsets>( x, n ) ;
            }
            case REALSXP: {
                    if( Rf_inherits(x, "difftime" )) return new ConstantDifftimeGatherer<REALSXP,Data,Subsets>(x,n) ;
                    if( Rf_inherits(x, "POSIXct" )) return new ConstantTypedGatherer<REALSXP,Data,Subsets>(x,n, get_time_classes() ) ;
                    if( Rf_inherits(x, "Date" )) return new ConstantTypedGatherer<REALSXP,Data,Subsets>(x,n, get_date_classes() ) ;
                    return new ConstantGathererImpl<REALSXP,Data,Subsets>( x, n ) ;
            }
            case LGLSXP: return new ConstantGathererImpl<LGLSXP,Data,Subsets>( x, n ) ;
            case STRSXP: return new ConstantGathererImpl<STRSXP,Data,Subsets>( x, n ) ;
            case VECSXP: return new ConstantGathererImpl<STRSXP,Data,Subsets>( x, n ) ;
            default: break ;
        }
        return 0 ;
    }
    
    template <typename Data, typename Subsets>
    inline Gatherer* gatherer( GroupedCallProxy<Data,Subsets>& proxy, const Data& gdf, SEXP name ){
        typename Data::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        Shield<SEXP> first( proxy.get(indices) ) ;
        switch( TYPEOF(first) ){
            case INTSXP:  
                {
                    if( Rf_inherits(first, "Date") ) return new TypedGatherer<INTSXP,Data,Subsets>(first, indices, proxy, gdf, get_date_classes() ) ;
                    return new GathererImpl<INTSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
                }
            case REALSXP:
                {
                    if( Rf_inherits(first, "POSIXct" ) ) return new TypedGatherer<REALSXP,Data,Subsets>(first, indices, proxy, gdf, get_time_classes() ) ;
                    if( Rf_inherits(first, "Date") ) return new TypedGatherer<REALSXP,Data,Subsets>(first, indices, proxy, gdf, get_date_classes() ) ;
                    return new GathererImpl<REALSXP,Data,Subsets>( first, indices, proxy, gdf ) ;
                }
            case LGLSXP:  return new GathererImpl<LGLSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case STRSXP:  return new GathererImpl<STRSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            case VECSXP:  return new GathererImpl<VECSXP,Data,Subsets> ( first, indices, proxy, gdf ) ;
            default: break ;
        }
        
        check_supported_type(first, name) ;
        return 0; 
    }
    
} // namespace dplyr


#endif
