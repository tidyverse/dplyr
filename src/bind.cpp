#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

class DataFrameAbleVector {
public:

  DataFrameAbleVector() : data() {}

  inline void push_back( SEXP x) {
    data.push_back( DataFrameAble(x) ) ;
  }

  inline const DataFrameAble& operator[]( int i) const {
    return data[i] ;
  }

  inline int size() const {
    return data.size() ;
  }

  ~DataFrameAbleVector() {
    while (data.size()) data.pop_back();
  }

private:
  std::vector<DataFrameAble> data ;
} ;

template <typename Dots>
List rbind__impl( Dots dots, SEXP id = R_NilValue ) {
  int ndata = dots.size() ;
  int n = 0 ;
  DataFrameAbleVector chunks ;
  std::vector<int> df_nrows ;

  int k=0 ;
  for( int i=0; i<ndata; i++) {
    SEXP obj = dots[i] ;
    if( Rf_isNull(obj) ) continue ;
    chunks.push_back( obj ) ;
    int nrows = chunks[k].nrows() ;
    df_nrows.push_back(nrows) ;
    n += nrows ;
    k++ ;
  }
  ndata = chunks.size() ;
  pointer_vector<Collecter> columns ;

  std::vector<String> names ;

  k=0 ;
  Function enc2native( "enc2native" ) ;
  for( int i=0; i<ndata; i++) {
    Rcpp::checkUserInterrupt() ;

    const DataFrameAble& df = chunks[i] ;
    if( !df.size() ) continue ;

    int nrows = df.nrows() ;

    CharacterVector df_names = enc2native(df.names()) ;
    for( int j=0; j<df.size(); j++) {
      SEXP source = df.get(j) ;
      String name = df_names[j] ;

      Collecter* coll = 0;
      size_t index = 0 ;
      for( ; index < names.size(); index++) {
        if( name == names[index] ) {
          coll = columns[index] ;
          break ;
        }
      }
      if( ! coll ) {
        coll = collecter( source, n ) ;
        columns.push_back( coll );
        names.push_back(name) ;
      }
      if( coll->compatible(source) ) {
        // if the current source is compatible, collect
        coll->collect( SlicingIndex( k, nrows), source ) ;

      } else if( coll->can_promote(source) ) {
        // setup a new Collecter
        Collecter* new_collecter = promote_collecter(source, n, coll ) ;

        // import data from this chunk
        new_collecter->collect( SlicingIndex( k, nrows), source ) ;

        // import data from previous collecter
        new_collecter->collect( SlicingIndex(0, k), coll->get() ) ;

        // dispose the previous collecter and keep the new one.
        delete coll ;
        columns[index] = new_collecter ;

      } else if( all_na(source) ) {
        // do nothing, the collecter already initialized data with the
        // right NA
      } else if( coll->is_logical_all_na()  ) {
        Collecter* new_collecter = collecter( source, n ) ;
        new_collecter->collect( SlicingIndex(k, nrows), source ) ;
        delete coll ;
        columns[index] = new_collecter ;
      } else {
        std::string column_name(name) ;
        stop(
          "Can not automatically convert from %s to %s in column \"%s\".",
          coll->describe(), get_single_class(source), column_name
        ) ;
      }

    }

    k += nrows ;
  }

  int nc = columns.size() ;
  int has_id = Rf_isNull(id) ? 0 : 1;

  List out(nc + has_id) ;
  CharacterVector out_names(nc + has_id) ;
  for( int i=0; i<nc; i++) {
    out[i + has_id] = columns[i]->get() ;
    out_names[i + has_id] = names[i] ;
  }

  // Add vector of identifiers if .id is supplied
  if (!Rf_isNull(id)) {
    CharacterVector df_names = dots.names() ;
    CharacterVector id_col = no_init(n) ;

    CharacterVector::iterator it = id_col.begin() ;
    for (int i=0; i<ndata; ++i) {
      std::fill( it, it + df_nrows[i], df_names[i] ) ;
      it += df_nrows[i] ;
    }
    out[0] = id_col ;
    out_names[0] = Rcpp::as<std::string>(id) ;
  }
  out.attr( "names" ) = out_names ;
  set_rownames( out, n ) ;

  // infer the classes and extra info (groups, etc ) from the first (#1692)
  if( ndata ) {
    const DataFrameAble& first = chunks[0] ;
    if( first.is_dataframe() ) {
      DataFrame df = first.get() ;
      out.attr("class") = df.attr("class") ;
      if( df.inherits("grouped_df") ) {
        out.attr("vars") = df.attr("vars") ;
        out = GroupedDataFrame(out).data() ;
      }
    } else {
      out.attr( "class" ) = classes_not_grouped() ;
    }
  } else {
    out.attr( "class" ) = classes_not_grouped() ;
  }

  return out ;
}

// [[Rcpp::export]]
List bind_rows_( List dots, SEXP id = R_NilValue ) {
  return rbind__impl(dots, id) ;
}

// [[Rcpp::export]]
List rbind_list__impl( Dots dots ) {
  return rbind__impl(dots) ;
}

template <typename Dots>
List cbind__impl( Dots dots ) {
  int n = dots.size() ;

  DataFrameAbleVector chunks ;
  for( int i=0; i<n; i++) {
    SEXP obj = dots[i] ;
    if( !Rf_isNull(obj) )
      chunks.push_back( dots[i] );
  }
  n = chunks.size() ;

  // first check that the number of rows is the same
  const DataFrameAble& df = chunks[0] ;
  int nrows = df.nrows() ;
  int nv = df.size() ;
  for( int i=1; i<n; i++) {
    const DataFrameAble& current = chunks[i] ;
    if( current.nrows() != nrows ) {
      stop( "incompatible number of rows (%d, expecting %d)", current.nrows(), nrows ) ;
    }
    nv += current.size() ;
  }

  // collect columns
  List out(nv) ;
  CharacterVector out_names(nv) ;

  // then do the subsequent dfs
  for( int i=0, k=0 ; i<n; i++) {
    Rcpp::checkUserInterrupt() ;

    const DataFrameAble& current = chunks[i] ;
    CharacterVector current_names = current.names() ;
    int nc = current.size() ;
    for( int j=0; j<nc; j++, k++) {
      out[k] = shared_SEXP(current.get(j)) ;
      out_names[k] = current_names[j] ;
    }
  }

  // infer the classes and extra info (groups, etc ) from the first (#1692)
  if( n ) {
    const DataFrameAble& first = chunks[0] ;
    if( first.is_dataframe() ) {
      DataFrame df = first.get() ;
      copy_most_attributes(out, df) ;
    } else {
      out.attr( "class" ) = classes_not_grouped() ;
    }
  } else {
    out.attr( "class" ) = classes_not_grouped() ;
  }
  out.names() = out_names ;
  set_rownames( out, nrows ) ;

  return out ;
}

// [[Rcpp::export]]
List cbind_all( List dots ) {
  return cbind__impl( dots ) ;
}

// [[Rcpp::export]]
SEXP combine_all( List data ) {
  int nv = data.size() ;
  if( nv == 0 ) stop("combine_all needs at least one vector") ;

  // get the size of the output
  int n = 0 ;
  for( int i=0; i<nv; i++) {
    n += Rf_length(data[i]) ;
  }

  // go to the first non NULL
  int i=0;
  for( ; i<nv; i++) {
    if( !Rf_isNull(data[i]) ) break ;
  }
  if( i == nv) stop( "no data to combine, all elements are NULL" ) ;

  // collect
  boost::scoped_ptr<Collecter> coll( collecter( data[i], n ) ) ;
  int k = Rf_length(data[i]) ;
  coll->collect( SlicingIndex(0, k), data[i] ) ;
  i++;
  for(; i<nv; i++) {
    SEXP current = data[i] ;
    if( Rf_isNull(current)) continue ;
    int n_current= Rf_length(current) ;

    if( coll->compatible(current) ) {
      coll->collect( SlicingIndex(k, n_current), current ) ;
    } else if( coll->can_promote(current) ) {
      Collecter* new_coll = promote_collecter(current, n, coll.get() ) ;
      new_coll->collect( SlicingIndex(k, n_current), current ) ;
      new_coll->collect( SlicingIndex(0, k), coll->get() ) ;
      coll.reset( new_coll ) ;
    } else {
      stop(
        "Can not automatically convert from %s to %s.",
        get_single_class(coll->get()), get_single_class(current)
      ) ;
    }
    k += n_current ;
  }

  return coll->get() ;
}
