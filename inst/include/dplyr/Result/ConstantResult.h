#ifndef dplyr_Result_ConstantResult_H
#define dplyr_Result_ConstantResult_H

namespace dplyr {
    
    template <int RTYPE>
    class ConstantResult : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        ConstantResult(SEXP x) : value( Rcpp::internal::r_vector_start<RTYPE>(x)[0] ) {}
        
        SEXP process( const GroupedDataFrame& gdf) {
            return Vector<RTYPE>( gdf.ngroups(), value ) ;        
        }
        
        virtual SEXP process( const FullDataFrame& df) {
            return Vector<RTYPE>::create( value ) ;    
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            return Vector<RTYPE>::create( value ) ;    
        }
        
        STORAGE value ;
    } ;
}

#endif
