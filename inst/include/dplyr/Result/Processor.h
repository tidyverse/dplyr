#ifndef dplyr_Result_Processor_H
#define dplyr_Result_Processor_H

namespace dplyr{
    
    // if we derive from this instead of deriving from Result, all we have to 
    // do is implement a process_chunk method that takes a Index_0_based& as
    // input and returns the suitable type (i.e. storage_type<OUTPUT>)
    // all the builtin result implementation (Mean, ...) use this. 
    template <int OUTPUT, typename CLASS>
    class Processor : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<OUTPUT>::type STORAGE ;
         
        Processor(){}
        
        virtual SEXP process(const Rcpp::GroupedDataFrame& gdf ) {
            int n = gdf.ngroups() ; 
            Rcpp::Shield<SEXP> res( Rf_allocVector( OUTPUT, n) );
            STORAGE* ptr = Rcpp::internal::r_vector_start<OUTPUT>(res) ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++)
                ptr[i] = obj->process_chunk(gdf.group(i)) ;
            return res ;        
        }
        
        virtual SEXP process( const Rcpp::FullDataFrame& df){
            CLASS* obj = static_cast<CLASS*>(this) ;
            return Rcpp::Vector<OUTPUT>::create( obj->process_chunk(df.get_index()) );    
        }
    } ;
    
    template <typename CLASS>
    class Processor<STRSXP, CLASS> : public Result {
    public:
        Processor(){}
        
        virtual SEXP process(const Rcpp::GroupedDataFrame& gdf) {
            int n = gdf.ngroups() ; 
            Rcpp::Shield<SEXP> res( Rf_allocVector( STRSXP, n) ) ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++)
                SET_STRING_ELT( res, i, obj->process_chunk(gdf.group(i)) );
            return res ;        
        }
        
        virtual SEXP process( const Rcpp::FullDataFrame& df){
            CLASS* obj = static_cast<CLASS*>(this) ;
            return Rf_mkString( obj->process_chunk(df.get_index()) );    
        }
        
    } ;

}
#endif
