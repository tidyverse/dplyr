#ifndef dplyr_Result_TypedProcessor_H
#define dplyr_Result_TypedProcessor_H

namespace dplyr {
          
  template <typename Processor>
  class TypedProcessor : public Result {
  public:
    typedef typename Processor::STORAGE STORAGE ;
    
    TypedProcessor( SEXP data, const char* cl ) : proc(data), classes(cl) {}
    TypedProcessor( SEXP data, CharacterVector classes_ ) : proc(data), classes(classes_) {}
    
    virtual SEXP process(const Rcpp::GroupedDataFrame& gdf ) {
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
    CharacterVector classes ;  
    
    inline SEXP promote(SEXP obj){
      RObject res(obj) ;
      res.attr("class") = classes ;
      return res ;
    }
     
  } ;                 
  
}

#endif
