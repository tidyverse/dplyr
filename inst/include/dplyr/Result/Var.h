#ifndef dplyr_Result_Var_H
#define dplyr_Result_Var_H

namespace dplyr {
namespace internal{
    inline double square(double x){ return x*x ; }    
}
    
    // version for NA_RM = false
    template <int RTYPE, bool NA_RM>
    class Var : public Processor<REALSXP, Var<RTYPE,NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Var(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Var(){}
        
        inline double process_chunk( const Index_0_based& indices ){
            int n=indices.size() ;
            if( n == 1 ) return NA_REAL ;
            double m = internal::mean<RTYPE,NA_RM>( data_ptr, indices ); 
            
            if( !R_FINITE(m) ) return m ;
            
            double sum = 0.0 ;
            for( int i=0; i<n; i++){
                sum += internal::square( data_ptr[indices[i]] - m ) ; 
            }
            return sum / ( n - 1 );
        }
         
    private:
        STORAGE* data_ptr ;
    } ;
    
    
    // version for NA_RM = true
    template <int RTYPE>
    class Var<RTYPE,true> : public Processor<REALSXP, Var<RTYPE,true> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Var(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Var(){}
        
        inline double process_chunk( const Index_0_based& indices ){
            int n=indices.size() ;
            if( n == 1 ) return NA_REAL ; 
            double m = internal::mean<RTYPE,true>( data_ptr, indices ); 
            
            if( !R_FINITE(m) ) return m ;
            
            double sum = 0.0 ;
            int count = 0 ;
            for( int i=0; i<n; i++){
                STORAGE current = data_ptr[indices[i]] ;
                if( Rcpp::Vector<RTYPE>::is_na(current) ) continue ;
                sum += internal::square( current - m ) ;
                count++ ;
            }
            if( count == 1 ) return NA_REAL ;
            return sum / ( count - 1 );
        }
         
    private:
        STORAGE* data_ptr ;
    } ;
    
    
    

}

#endif
