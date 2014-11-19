#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

namespace dplyr{
    
    template <typename CLASS, typename Data>
    class DelayedProcessor_Base {
       public:
           typedef typename Data::group_iterator group_iterator ;
        
           DelayedProcessor_Base(){}
           virtual ~DelayedProcessor_Base(){}
           
           virtual SEXP delayed_process( const Data& map, SEXP first_result, CLASS*, group_iterator git ) = 0;
    } ;
    
    template <int RTYPE>
    typename Rcpp::traits::storage_type<RTYPE>::type strong_as( SEXP x ){
        return as< typename Rcpp::traits::storage_type<RTYPE>::type >(x) ;   
    }
    
    template <>
    inline int strong_as<INTSXP>( SEXP x){
        if( TYPEOF(x) == REALSXP ){
            std::stringstream s ; 
            s << "loss of precision when attempting to convert a "  
              << get_single_class(x) 
              << " to an integer" ;
            stop(s.str());
        }
        return as<int>(x) ;
    }
    
    template <>
    inline int strong_as<LGLSXP>( SEXP x){
        if( TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP ){
            std::stringstream s ; 
            s << "loss of precision when attempting to convert a "  
              << get_single_class(x) 
              << " to an logical" ;
            stop(s.str());
        }
        return as<int>(x) ;
    }
    
    template <int RTYPE, typename CLASS, typename Data>
    class DelayedProcessor : public DelayedProcessor_Base<CLASS, Data> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef Vector<RTYPE> Vec ;
        typedef typename Data::group_iterator group_iterator ;
        
        DelayedProcessor(int first_non_na_) : first_non_na(first_non_na_){}
        
        virtual SEXP delayed_process(const Data& gdf, SEXP first_result, CLASS* obj, group_iterator git) {
            
            int n = gdf.ngroups() ; 
            Vector<RTYPE> res = no_init(n) ;
            STORAGE* ptr = res.begin() ;
            
            int i=0 ;
            for( ; i<first_non_na; i++){ 
                ptr[i] = Vec::get_na() ;   
            }
            ptr[i] = strong_as<RTYPE>( first_result );
            ++git ;
            i++ ;
            for( ; i<n; i++, ++git )
                ptr[i] = strong_as<RTYPE>( obj->process_chunk(*git) ) ;
            return res ;        
        }
         
    private:
        int first_non_na ;
    } ;
    
    template <typename CLASS, typename Data>
    class DelayedProcessor<STRSXP, CLASS, Data> : public DelayedProcessor_Base<CLASS, Data> {
    public:
        typedef typename Data::group_iterator group_iterator ;
        
        DelayedProcessor(int first_non_na_) : first_non_na(first_non_na_){}
        
        virtual SEXP delayed_process(const Data& gdf, SEXP first_result, CLASS* obj, group_iterator git) {
            int n = gdf.ngroups() ; 
            CharacterVector res(n) ;
            int i=0 ;
            for( ; i<first_non_na; i++) res[i] = NA_STRING ;
            res[i] = STRING_ELT(first_result, 0 ) ;
            ++git ;
            i++ ;
            for( ; i<n; i++, ++git )
                res[i] = STRING_ELT( obj->process_chunk(*git), 0) ;
            return res ;        
        }
       
    private:
        int first_non_na ;
    } ;
    
    template <typename CLASS, typename Data>
    DelayedProcessor_Base<CLASS, Data>* get_delayed_processor(SEXP first_result, int i){
        if( Rcpp::is<int>( first_result ) ){       
            return new DelayedProcessor<INTSXP, CLASS, Data>(i) ;    
        } else if( Rcpp::is<double>( first_result) ){
            return new DelayedProcessor<REALSXP, CLASS, Data>(i) ;    
        } else if( Rcpp::is<Rcpp::String>( first_result) ){
            return new DelayedProcessor<STRSXP, CLASS, Data>(i) ;
        } else if( Rcpp::is<bool>( first_result) ){
            return new DelayedProcessor<LGLSXP, CLASS, Data>(i) ;
        }
        return 0 ;
    }

}
#endif
