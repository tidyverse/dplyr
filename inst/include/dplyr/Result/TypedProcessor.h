#ifndef dplyr_Result_TypedProcessor_H
#define dplyr_Result_TypedProcessor_H

namespace dplyr {
                    
    template <typename Processor>
    class TypedProcessor : public Result {
    public:
        TypedProcessor( Processor proc_, SEXP data_) :
            proc(proc_), data(data_){}
            
        virtual SEXP process(const Rcpp::GroupedDataFrame& gdf ) {
            return promote( proc.process(gdf) ) ;
        }
        
        virtual SEXP process(const Rcpp::RowwiseDataFrame& gdf ) {
            return promote( proc.process(gdf) ) ;
        }
        
        virtual SEXP process( const Rcpp::FullDataFrame& df){
            return promote( proc.process(df) ) ;
        }
        
        virtual SEXP process( const SlicingIndex& index){
            return promote( proc.process(index) ) ;    
        }
        
    private:
        Processor proc ;
        SEXP data ;
        
        inline SEXP promote(SEXP obj){
            RObject res(obj) ;
            copy_attributes(res, data) ; 
            return res ;
        }
         
    } ;                                 
    
    template <typename Processor>
    inline TypedProcessor<Processor>* typed_processor( const Processor& proc, SEXP data ){
        return new TypedProcessor<Processor>(proc, data );
    }
    
    
}

#endif
