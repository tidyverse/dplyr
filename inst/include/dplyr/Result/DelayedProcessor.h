#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

namespace dplyr{
    
    template <typename CLASS, typename Data>
    class DelayedProcessor_Base {
       public:
           DelayedProcessor_Base(){}
           virtual ~DelayedProcessor_Base(){}
           
           virtual SEXP delayed_process( const Data& map, SEXP first_result, CLASS* ) = 0;
    } ;
    
    template <int RTYPE, typename CLASS, typename Data>
    class DelayedProcessor : public DelayedProcessor_Base<CLASS, Data> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
         
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const Data& gdf, SEXP first_result, CLASS* obj) {
            int n = gdf.ngroups() ; 
            Shield<SEXP> res( Rf_allocVector( RTYPE, n) ) ;
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>(res) ;
            ptr[0] = Rcpp::as<STORAGE>( first_result );
            typename Data::group_iterator git = gdf.group_begin() ;
            ++git ;
            for( int i=1; i<n; i++, ++git )
                ptr[i] = Rcpp::as<STORAGE>( obj->process_chunk(*git) ) ;
            return res ;        
        }
                      
    } ;
    
    template <typename CLASS, typename Data>
    class DelayedProcessor<STRSXP, CLASS, Data> : public DelayedProcessor_Base<CLASS, Data> {
    public:
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const Data& gdf, SEXP first_result, CLASS* obj) {
            int n = gdf.ngroups() ; 
            Shield<SEXP> res( Rf_allocVector( STRSXP, n) ) ;
            SET_STRING_ELT( res, 0, STRING_ELT(first_result, 0 ) ) ;
            typename Data::group_iterator git = gdf.group_begin() ;
            ++git ;
            for( int i=1; i<n; i++, ++git )
                SET_STRING_ELT( res, i, STRING_ELT( obj->process_chunk(*git), 0) ) ;
            return res ;        
        }
        
    } ;
    
    template <typename CLASS, typename Data>
    DelayedProcessor_Base<CLASS, Data>* get_delayed_processor(SEXP first_result){
        if( Rcpp::is<int>( first_result ) ){       
            return new DelayedProcessor<INTSXP, CLASS, Data>() ;    
        } else if( Rcpp::is<double>( first_result) ){
            return new DelayedProcessor<REALSXP, CLASS, Data>() ;    
        } else if( Rcpp::is<Rcpp::String>( first_result) ){
            return new DelayedProcessor<STRSXP, CLASS, Data>() ;
        } else if( Rcpp::is<bool>( first_result) ){
            return new DelayedProcessor<LGLSXP, CLASS, Data>() ;
        }
        return 0 ;
    }

}
#endif
