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
    
    template <int RTYPE, typename Derived>
    class TypedConstantResult : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        TypedConstantResult(SEXP x) : value( Rcpp::internal::r_vector_start<RTYPE>(x)[0] ) {}
        
        SEXP process( const GroupedDataFrame& gdf) {
            return get(gdf.ngroups()) ;        
        }
        
        virtual SEXP process( const FullDataFrame& df) {
            return get(1);    
        }
        
        virtual SEXP process( const SlicingIndex& index ){
            return get(1);     
        }
        
    private:
        
        SEXP get( int n ) const {
            Vector<RTYPE> res(n, value);
            res.attr("class") = static_cast<const Derived&>(*this).classes() ;
            return res ;
        }
        
        STORAGE value ;
    } ;
    
    class POSIXctConstantResult : public TypedConstantResult<REALSXP,POSIXctConstantResult>{
        public:
           typedef TypedConstantResult<REALSXP,POSIXctConstantResult> Base ;
           POSIXctConstantResult(SEXP x): Base(x){}
           
           inline CharacterVector classes() const {
               return CharacterVector::create( "POSIXct", "POSIXt" ) ;
           }
    } ;
    
    class DateConstantResult : public TypedConstantResult<REALSXP,DateConstantResult>{
        public:
           typedef TypedConstantResult<REALSXP,DateConstantResult> Base ;
           DateConstantResult(SEXP x): Base(x){}
           
           inline CharacterVector classes() const {
               return CharacterVector::create( "Date" ) ;
           }
    } ;
    
}

#endif
