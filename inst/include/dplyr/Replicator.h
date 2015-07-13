#ifndef dplyr_Replicator_H
#define dplyr_Replicator_H

namespace dplyr {
    
    class Replicator {
    public:
        virtual ~Replicator(){}
        virtual SEXP collect() = 0 ;
    } ;
        
    template <int RTYPE, typename Data>
    class ReplicatorImpl : public Replicator {
    public:
        typedef typename traits::storage_type<RTYPE>::type STORAGE ;
        
        ReplicatorImpl( SEXP v, int n_, int ngroups_) : 
            data( no_init(n_*ngroups_) ), source(v), n(n_), ngroups(ngroups_) {}
            
        SEXP collect(){ 
            for( int i=0, k=0; i<ngroups; i++){
                for( int j=0; j<n; j++, k++){
                    data[k] = source[j] ;   
                }
            }
            return data ;
        }
    
    private: 
        Vector<RTYPE> data ;
        Vector<RTYPE> source ;
        int n ;
        int ngroups ; 
    } ;   
    
    template <int RTYPE, typename Data>
    class TypedReplicator : public ReplicatorImpl<RTYPE, Data> {
    public:
        typedef ReplicatorImpl<RTYPE, Data> Base ;
        
        TypedReplicator( SEXP v, int n_, int ngroups_, SEXP classes_) : 
          Base(v,n_,ngroups_), classes(classes_) {}
        
        SEXP collect(){
            Vector<RTYPE> res( Base::collect() ) ;
            res.attr( "class" ) = classes ;
            return res ;
        }
        
    private:
        SEXP classes ;        
    } ;
    
    template <int RTYPE, typename Data>
    class DifftimeReplicator : public ReplicatorImpl<RTYPE, Data> {
    public:
        typedef ReplicatorImpl<RTYPE, Data> Base ;
        
        DifftimeReplicator( SEXP v, int n_, int ngroups_) : 
            Base(v,n_,ngroups_), 
            units(Rf_getAttrib(v, Rf_install("units")))
        {}
        
        SEXP collect(){
            Vector<RTYPE> res( Base::collect() ) ;
            res.attr( "class" ) = "difftime" ;
            res.attr( "units" ) = units ;
            return res ;
        }
        
    private:
        CharacterVector units ;        
    } ;
    
    template <typename Data>
    inline Replicator* replicator( SEXP v, const Data& gdf ){
        int n = Rf_length(v) ;
        bool test = all( gdf.get_group_sizes() == n ).is_true() ;
        if( !test ){
            stop( "impossible to replicate vector of size %s", n );
        }
                      
        switch( TYPEOF(v) ){
            case INTSXP:  
                {
                    if( Rf_inherits( v, "Date" ) ) return new TypedReplicator<INTSXP, Data>(v, n, gdf.ngroups(), get_date_classes() ) ;
                    return new ReplicatorImpl<INTSXP, Data> ( v, n, gdf.ngroups() ) ;
                }
            case REALSXP: {
                    if( Rf_inherits( v, "difftime" ) ) return new DifftimeReplicator<REALSXP, Data>(v, n, gdf.ngroups() ) ;
                    if( Rf_inherits( v, "POSIXct" ) ) return new TypedReplicator<REALSXP, Data>(v, n, gdf.ngroups(), get_time_classes() ) ;
                    if( Rf_inherits( v, "Date" ) ) return new TypedReplicator<REALSXP, Data>(v, n, gdf.ngroups(), get_date_classes() ) ;
                    return new ReplicatorImpl<REALSXP, Data>( v, n, gdf.ngroups() ) ;
            }
            case STRSXP:  return new ReplicatorImpl<STRSXP, Data> ( v, n, gdf.ngroups() ) ;
            case LGLSXP:  return new ReplicatorImpl<LGLSXP, Data> ( v, n, gdf.ngroups() ) ;
            default: break ;
        }
        stop( "cannot handle variable" ) ;
        
        return 0 ;
    }
    
} // namespace dplyr


#endif
