#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

namespace dplyr{
    
    template <typename CLASS>
    class DelayedProcessor_Base {
       public:
           DelayedProcessor_Base(){}
           virtual ~DelayedProcessor_Base(){}
           
           virtual SEXP delayed_process( const Rcpp::GroupedDataFrame& map, SEXP first_result, CLASS* ) = 0;
    } ;
    
    template <int RTYPE, typename CLASS>
    class DelayedProcessor : public DelayedProcessor_Base<CLASS> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
         
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const Rcpp::GroupedDataFrame& gdf, SEXP first_result, CLASS* obj) {
            Rcpp::Shelter<SEXP> __ ;
            int n = gdf.ngroups() ; 
            SEXP res = __( Rf_allocVector( RTYPE, n) ) ;
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>(res) ;
            ptr[0] = Rcpp::as<STORAGE>( first_result );
            for( int i=1; i<n; i++ )
                ptr[i] = Rcpp::as<STORAGE>( obj->process_chunk(gdf.group(i)) ) ;
            return res ;        
        }
                      
    } ;
    
    template <typename CLASS>
    class DelayedProcessor<STRSXP, CLASS> : public DelayedProcessor_Base<CLASS> {
    public:
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const Rcpp::GroupedDataFrame& gdf, SEXP first_result, CLASS* obj) {
            Rcpp::Shelter<SEXP> __ ;
            int n = gdf.ngroups() ; 
            SEXP res = __( Rf_allocVector( STRSXP, n) ) ;
            SET_STRING_ELT( res, 0, STRING_ELT(first_result, 0 ) ) ;
            for( int i=1; i<n; i++ )
                SET_STRING_ELT( res, i, STRING_ELT( obj->process_chunk(gdf.group(i)), 0) ) ;
            return res ;        
        }
        
    } ;
    
    template <typename CLASS>
    DelayedProcessor_Base<CLASS>* get_delayed_processor(SEXP first_result){
        if( Rcpp::is<int>( first_result ) ){       
            return new DelayedProcessor<INTSXP, CLASS>() ;    
        } else if( Rcpp::is<double>( first_result) ){
            return new DelayedProcessor<REALSXP, CLASS>() ;    
        } else if( Rcpp::is<Rcpp::String>( first_result) ){
            return new DelayedProcessor<STRSXP, CLASS>() ;
        }
        return 0 ;
    }

}
#endif
