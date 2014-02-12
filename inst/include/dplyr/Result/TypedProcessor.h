#ifndef dplyr_Result_TypedProcessor_H
#define dplyr_Result_TypedProcessor_H

namespace dplyr {
          
  template <typename Processor>
  class TypedProcessor : public Result {
  public:
    typedef typename Processor::STORAGE STORAGE ;
    
    TypedProcessor( SEXP data_, const char* cl ) : proc(data_), classes(cl), data(data_) {}
    TypedProcessor( SEXP data_, CharacterVector classes_ ) : proc(data_), classes(classes_), data(data_) {}
    
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
    SEXP data ;
    
    inline SEXP promote(SEXP obj){
      RObject res(obj) ;
      copy_attributes(res, data) ; 
      res.attr("class") = classes ;
      return res ;
    }
     
  } ;                 
  
}

#endif
