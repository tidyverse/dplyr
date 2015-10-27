#ifndef dplyr_DataFrameAble_H
#define dplyr_DataFrameAble_H

namespace dplyr {

  class DataFrameAbleImpl {
  public:
    virtual ~DataFrameAbleImpl(){} ;
    virtual int nrows() const = 0 ;
    virtual SEXP get( int i ) const = 0 ;
    virtual int size() const = 0 ;
    virtual CharacterVector names() const = 0 ;
  } ;

  class DataFrameAble_DataFrame : public DataFrameAbleImpl {
  public:
    DataFrameAble_DataFrame( DataFrame data_) : data(data_){
      if( data.size() ){
        CharacterVector df_names = data.names() ;
        if( any(is_na(df_names)).is_true() ){
          stop( "corrupt data frame" ) ;
        }
      }
    }

    inline int nrows() const {
      return data.nrows() ;
    }

    inline SEXP get(int i) const {
      return data[i] ;
    }

    inline int size() const {
      return data.size() ;
    }

    inline CharacterVector names() const {
      return data.names() ;
    }

  private:
    DataFrame data ;
  } ;

  class DataFrameAble_Null : public DataFrameAbleImpl {
  public:
    DataFrameAble_Null(){}
    inline int nrows() const { return 0 ;}
    inline SEXP get(int i) const { return R_NilValue ;}
    inline int size() const { return 0 ;}
    inline CharacterVector names() const { return R_NilValue ; }
  } ;

  class DataFrameAble_List : public DataFrameAbleImpl {
  public:
    DataFrameAble_List( SEXP data_) : data(data_), nr(0){
      int n = data.size() ;
      if( data.size() == 0) return ;
      nr = Rf_length(data[0]) ;
      for(int i=1; i<n; i++){
        if( Rf_length(data[i]) != nr ) {
          stop( "incompatible sizes (%d != %s)", nr, Rf_length(data[i]) ) ;
        }
      }
    }

    inline int nrows() const {
      return nr ;
    }

    inline SEXP get(int i) const {
      return data[i] ;
    }

    inline int size() const {
      return data.size() ;
    }

    inline CharacterVector names() const {
      return data.names() ;
    }

  private:
    List data ;
    int nr ;
  } ;

  class DataFrameAble{
  public:
    DataFrameAble( SEXP data ) {
      init(data) ;
    }
    DataFrameAble( List::Proxy data){
      init( (SEXP)data) ;
    }


    inline int nrows() const {
      return impl->nrows() ;
    }

    inline int size() const {
      return impl->size() ;
    }

    inline SEXP get( int i ) const {
      return impl->get(i) ;
    }

    inline CharacterVector names()  const {
      return impl->names() ;
    }

  private:
    boost::shared_ptr<DataFrameAbleImpl> impl ;

    inline void init( SEXP data){
      if( Rf_isNull(data) ){
        impl.reset( new DataFrameAble_Null() ) ;
      } else if( Rf_inherits( data, "data.frame")){
        impl.reset( new DataFrameAble_DataFrame(data)) ;
      } else if( is<List>(data) ){
        impl.reset( new DataFrameAble_List(data) ) ;
      } else {
        stop( "cannot convert object to a data frame" ) ;
      }
    }

  } ;

}


#endif
