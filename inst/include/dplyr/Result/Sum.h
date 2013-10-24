#ifndef dplyr_Result_Sum_H
#define dplyr_Result_Sum_H

namespace dplyr {
      
namespace internal {
    
    // this one is actually only used for RTYPE = REALSXP and NA_RM = true
    template <int RTYPE, bool NA_RM, typename Index> 
    struct Sum {
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        static STORAGE process(typename Rcpp::traits::storage_type<RTYPE>::type* ptr,  const Index& indices) {
            long double res = 0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                double value = ptr[indices[i]] ;
                if( ! Rcpp::traits::is_na<RTYPE>( value ) ) res += value ;    
            }
            return (double)res ;
        }
    } ;
    
    template <typename Index>
    struct Sum<INTSXP,true, Index> {
        static int process( int* ptr, const Index& indices){
            long double res = 0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                int value = ptr[indices[i]] ;
                if( ! Rcpp::traits::is_na<INTSXP>( value ) ) res += value ;    
            }
            if(res > INT_MAX || res <= INT_MIN){
                return IntegerVector::get_na() ;   
            }
            return (int)res ;    
        }
    };
    
    template <typename Index>
    struct Sum<INTSXP, false, Index>{
        static int process( int* ptr, const Index& indices ){
            int res = 0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                int value = ptr[indices[i]] ;
                if( Rcpp::traits::is_na<INTSXP>( value ) ){
                    return NA_INTEGER ;    
                }
                res += value ;    
            }
            return res ;         
        }
    } ;
    
    template <typename Index>
    struct Sum<REALSXP, false, Index> {
        static double process( double* ptr, const Index_0_based& indices ){
            long double res = 0.0 ;
            int n = indices.size() ;
            for( int i=0; i<n; i++){
                // we don't test for NA here because += NA will give NA
                // this is faster in the most common case where there are no NA
                // if there are NA, we could return quicker as in the version for
                // INTSXP above, but we would penalize the most common case
                res += ptr[ indices[i] ] ;    
            }
            return (double)res ;    
        }
    } ;
    
    
} // namespace internal
           
    template <int RTYPE, bool NA_RM>
    class Sum : public Processor< RTYPE, Sum<RTYPE,NA_RM> > {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Sum(SEXP x) : data_ptr( Rcpp::internal::r_vector_start<RTYPE>(x) ) {}
        ~Sum(){}
        
        inline STORAGE process_chunk( const Index_0_based& indices ){
            return internal::Sum<RTYPE,NA_RM,Index_0_based>::process(data_ptr, indices) ;    
        }
        
        STORAGE* data_ptr ;
    } ;

}

#endif
