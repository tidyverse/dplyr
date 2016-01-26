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

        GathererImpl( RObject& first, SlicingIndex& indices, Proxy& proxy_, const Data& gdf_, int first_non_na_ ) :
            gdf(gdf_), proxy(proxy_), data(gdf.nrows(), Vector<RTYPE>::get_na() ), first_non_na(first_non_na_)
        {
            grab( first, indices ) ;
            copy_most_attributes( data, first ) ;
        }

        SEXP collect(){
            int ngroups = gdf.ngroups() ;
            typename Data::group_iterator git = gdf.group_begin() ;
            int i = 0 ;
            for(; i<first_non_na; i++) ++git ;
            for(; i<ngroups; i++, ++git){
                SlicingIndex indices = *git ;
                Shield<SEXP> subset( proxy.get( indices ) ) ;
                grab(subset, indices);
            }
            return data ;
        }

    private:

        inline void grab(SEXP data, const SlicingIndex& indices){
            int n = Rf_length(data) ;
            if( is<LogicalVector>(data) && all(is_na(LogicalVector(data))).is_true() ){
              grab_rep( Vector<RTYPE>::get_na(), indices ) ;
            } else {
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
        int first_non_na ;

    } ;

    template <typename Data, typename Subsets>
    class FactorGatherer : public Gatherer {
    public:
        typedef GroupedCallProxy<Data,Subsets> Proxy ;
        typedef IntegerVector Factor;

        FactorGatherer( RObject& first, SlicingIndex& indices, Proxy& proxy_, const Data& gdf_, int first_non_na_ ) :
          levels(), data(gdf_.nrows()), first_non_na(first_non_na_), proxy(proxy_), gdf(gdf_)
        {
          grab( (SEXP)first, indices ) ;
          copy_most_attributes( data, first ) ;
        }

        inline SEXP collect(){
          int ngroups = gdf.ngroups() ;
          typename Data::group_iterator git = gdf.group_begin() ;
          int i = 0 ;
          for(; i<first_non_na; i++) ++git ;
          for(; i<ngroups; i++, ++git){
              SlicingIndex indices = *git ;
              Factor subset( proxy.get( indices ) ) ;
              grab(subset, indices);
          }
          CharacterVector levels_(levels_vector.begin(), levels_vector.end() ) ;
          data.attr("levels") = levels_ ;
          return data ;
        }

    private:
        dplyr_hash_map<SEXP, int> levels ;
        Factor data ;
        int first_non_na ;
        Proxy& proxy ;
        const Data& gdf ;
        std::vector<SEXP> levels_vector ;

        void grab( Factor f, const SlicingIndex& indices ){
            // update levels if needed
            CharacterVector lev = f.attr("levels") ;
            std::vector<int> matches( lev.size() ) ;
            int nlevels = levels.size() ;
            for( int i=0; i<lev.size(); i++){
                SEXP level = lev[i] ;
                if( !levels.count(level) ){
                    nlevels++ ;
                    levels_vector.push_back(level) ;
                    levels[level] = nlevels ;
                    matches[i] = nlevels ;
                } else {
                  matches[i] = levels[level] ;
                }
            }

            // grab data
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                if( f[i] == NA_INTEGER ){
                  data[ indices[i] ] = NA_INTEGER ;
                } else {
                  data[ indices[i] ] = matches[ f[i] - 1 ] ;
                }

            }
        }


    } ;

    template <int RTYPE>
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

    inline Gatherer* constant_gatherer(SEXP x, int n){
        if( Rf_inherits(x, "POSIXlt" ) ){
            stop("`mutate` does not support `POSIXlt` results");
        }
        switch( TYPEOF(x) ){
            case INTSXP:  return new ConstantGathererImpl<INTSXP>( x, n ) ;
            case REALSXP: return new ConstantGathererImpl<REALSXP>( x, n ) ;
            case LGLSXP:  return new ConstantGathererImpl<LGLSXP>( x, n ) ;
            case STRSXP:  return new ConstantGathererImpl<STRSXP>( x, n ) ;
            case CPLXSXP: return new ConstantGathererImpl<CPLXSXP>( x, n ) ;
            case VECSXP:  return new ConstantGathererImpl<STRSXP>( x, n ) ;
            default: break ;
        }
        return 0 ;
    }

    template <typename Data, typename Subsets>
    inline Gatherer* gatherer( GroupedCallProxy<Data,Subsets>& proxy, const Data& gdf, SEXP name ){
        typename Data::group_iterator git = gdf.group_begin() ;
        SlicingIndex indices = *git ;
        RObject first( proxy.get(indices) ) ;

        if( Rf_inherits(first, "POSIXlt" ) ){
            stop("`mutate` does not support `POSIXlt` results");
        }
        int ng = gdf.ngroups() ;
        int i = 1 ; ++git ;
        for( ; all_na(first) && i<ng; i++, ++git){
          indices = *git ;
          first = proxy.get(indices) ;
        }

        switch( TYPEOF(first) ){
            case INTSXP:
              {
                if( Rf_inherits(first, "factor"))
                  return new FactorGatherer<Data, Subsets>( first, indices, proxy, gdf, i) ;
                return new GathererImpl<INTSXP,Data,Subsets>  ( first, indices, proxy, gdf, i ) ;
              }
            case REALSXP: return new GathererImpl<REALSXP,Data,Subsets> ( first, indices, proxy, gdf, i ) ;
            case LGLSXP:  return new GathererImpl<LGLSXP,Data,Subsets>  ( first, indices, proxy, gdf, i ) ;
            case STRSXP:  return new GathererImpl<STRSXP,Data,Subsets>  ( first, indices, proxy, gdf, i ) ;
            case VECSXP:  return new GathererImpl<VECSXP,Data,Subsets>  ( first, indices, proxy, gdf, i ) ;
            case CPLXSXP: return new GathererImpl<CPLXSXP,Data,Subsets> ( first, indices, proxy, gdf, i ) ;
            default: break ;
        }
        check_supported_type(first, name) ;
        return 0;
    }

} // namespace dplyr


#endif
