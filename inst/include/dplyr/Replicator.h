#ifndef dplyr_Replicator_H
#define dplyr_Replicator_H

namespace dplyr {
    
    class Replicator {
    public:
        virtual ~Replicator(){}
        virtual SEXP collect() = 0 ;
    } ;
        
    template <int RTYPE>
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
    
    template <int RTYPE>
    class ConstantReplicatorImpl : public Replicator {
    public:
        typedef typename traits::storage_type<RTYPE>::type STORAGE ;
        
        ConstantReplicatorImpl( SEXP v, int n ) : 
            data(n, Rcpp::internal::r_vector_start<RTYPE>(v)[0]){}
              
        SEXP collect(){
            return data ;
        }
        
    private:
        Vector<RTYPE> data ;
    } ;
    
    inline Replicator* constant_replicator(SEXP v, const int n){
        switch( TYPEOF(v) ){
            case INTSXP:  return new ConstantReplicatorImpl<INTSXP>( v, n ) ;
            case REALSXP: return new ConstantReplicatorImpl<REALSXP>( v, n ) ;
            case STRSXP:  return new ConstantReplicatorImpl<STRSXP>( v, n ) ;
            case LGLSXP:  return new ConstantReplicatorImpl<LGLSXP>( v, n ) ;
            default: break ;
        }
        stop( "cannot handle variable" ) ;
        return 0 ;
    }
    
    inline Replicator* replicator( SEXP v, const GroupedDataFrame& gdf ){
        int n = Rf_length(v) ;
        bool test = all( gdf.get_group_sizes() == n ).is_true() ;
        if( !test ){
            std::stringstream s ;
            s << "impossible to replicate vector of size" << n ;
            stop(s.str()) ;
        }
                      
        switch( TYPEOF(v) ){
            case INTSXP:  return new ReplicatorImpl<INTSXP> ( v, n, gdf.ngroups() ) ;
            case REALSXP: return new ReplicatorImpl<REALSXP>( v, n, gdf.ngroups() ) ;
            case STRSXP:  return new ReplicatorImpl<STRSXP> ( v, n, gdf.ngroups() ) ;
            case LGLSXP:  return new ReplicatorImpl<LGLSXP> ( v, n, gdf.ngroups() ) ;
            default: break ;
        }
        stop( "cannot handle variable" ) ;
        
        return 0 ;
    }
    
} // namespace dplyr


#endif
