#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef dplyr_hash_map<SEXP,HybridHandler> HybridHandlerMap ;

template <template <int,bool> class Fun>
Result* simple_prototype(  SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs != 1 ) return 0 ;                                               
    SEXP arg = CADR(call) ;                                                   
    if( TYPEOF(arg) == SYMSXP ){
      if( subsets.count(arg) ) arg = subsets.get_variable(arg) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(arg) ){                                                    
        case INTSXP:  return new Fun<INTSXP,false>( arg ) ;      
        case REALSXP: return new Fun<REALSXP,false>( arg ) ;     
        default: break ;                                                      
    }                                                                         
    return 0 ;                                                                
}

template< template <int, bool> class Tmpl>
Result* minmax_prototype( SEXP call, const LazySubsets& subsets, int nargs ){
    using namespace dplyr ;

    if( nargs != 1 ) return 0 ;
    SEXP arg = CADR(call) ;
    if( TYPEOF(arg) == SYMSXP ){
      if( subsets.count(arg) ) arg = subsets.get_variable(arg) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(arg) ){
        case INTSXP:
            if( Rf_inherits(arg, "Date" ) )
                return new TypedProcessor< Tmpl<INTSXP,false> >( arg, "Date" ) ;
            if( Rf_inherits(arg, "POSIXct" ) )
                return new TypedProcessor< Tmpl<INTSXP,false> >( arg, CharacterVector::create( "POSIXct", "POSIXt" ) ) ;
            return new Tmpl<INTSXP,false>( arg ) ;
        case REALSXP:
            if( Rf_inherits(arg, "Date" ) )
                return new TypedProcessor< Tmpl<REALSXP,false> >( arg, "Date" ) ;
            if( Rf_inherits(arg, "POSIXct" ) )
                return new TypedProcessor< Tmpl<REALSXP,false> >( arg, CharacterVector::create( "POSIXct", "POSIXt" ) ) ;
            return new Tmpl<REALSXP,false>( arg ) ;
        default: break ;
    }
    return 0 ;
}

Result* count_distinct_result(SEXP vec){
    switch( TYPEOF(vec) ){
        case INTSXP:
            if( Rf_inherits(vec, "factor" ))
                return new Count_Distinct<FactorVisitor>( FactorVisitor(vec) ) ;
            if( Rf_inherits( vec, "Date" ) )
                return new Count_Distinct< DateVisitor<INTSXP> >( DateVisitor<INTSXP>(vec) ) ;
            if( Rf_inherits( vec, "POSIXct" ) )
                return new Count_Distinct<POSIXctVisitor<INTSXP> >( POSIXctVisitor<INTSXP>(vec) ) ;
              
            return new Count_Distinct< VectorVisitorImpl<INTSXP> >( VectorVisitorImpl<INTSXP>(vec) ) ;
        case REALSXP:
            if( Rf_inherits( vec, "Date" ) )
                return new Count_Distinct< DateVisitor<REALSXP> >( DateVisitor<REALSXP>(vec) ) ;
            if( Rf_inherits( vec, "POSIXct" ) )
                return new Count_Distinct<POSIXctVisitor<REALSXP> >( POSIXctVisitor<REALSXP>(vec) ) ;
            return new Count_Distinct< VectorVisitorImpl<REALSXP> >( VectorVisitorImpl<REALSXP>(vec) ) ;
        case LGLSXP:  return new Count_Distinct< VectorVisitorImpl<LGLSXP> >( VectorVisitorImpl<LGLSXP>(vec) ) ;
        case STRSXP:  return new Count_Distinct< VectorVisitorImpl<STRSXP> >( VectorVisitorImpl<STRSXP>(vec) ) ;
        default: break ;
    }
    return 0 ;
}

Result* count_prototype(SEXP args, const LazySubsets&, int){
    if( Rf_length(args) != 1)
        stop("n does not take arguments") ;
    return new Count ;
}

Result* count_distinct_prototype(SEXP call, const LazySubsets& subsets, int){
    SEXP arg = CADR(call) ;
    if( TYPEOF(arg) == SYMSXP ){
      if( subsets.count(arg) ) arg = subsets.get_variable(arg) ;                                       
      else return 0 ;
    }
    return count_distinct_result( arg ) ;
}

Result* row_number_prototype(SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs != 1) return 0;
    Armor<SEXP> data( CADR(call) );
    if( TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc") ){
        data = CADR(data) ;

        if( TYPEOF(data) == SYMSXP ){
          if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
          else return 0 ;
        }
        switch( TYPEOF(data) ){
            case INTSXP:  return new RowNumber<INTSXP,  false>( data ) ;
            case REALSXP: return new RowNumber<REALSXP, false>( data ) ;
            case STRSXP:  return new RowNumber<STRSXP,  false>( data ) ;
            default: break;
        }
    }
    if( TYPEOF(data) == SYMSXP ){
      if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(data) ){
        case INTSXP:  return new RowNumber<INTSXP,true>( data ) ;
        case REALSXP: return new RowNumber<REALSXP,true>( data ) ;
        case STRSXP: return new RowNumber<STRSXP,true>( data ) ;
        default: break;
    }
    // we don't know how to handle it.
    return 0 ;
}

template <typename Increment>
Result* rank_impl_prototype(SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs != 1) return 0;
    Armor<SEXP> data( CADR(call) );

    if( TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc") ){
        data = CADR(data) ;
        if( TYPEOF(data) == SYMSXP ){
          if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
          else return 0 ;
        }
    
        switch( TYPEOF(data) ){
            case INTSXP:  return new Rank_Impl<INTSXP,  Increment, false>( data ) ;
            case REALSXP: return new Rank_Impl<REALSXP, Increment, false>( data ) ;
            case STRSXP:  return new Rank_Impl<STRSXP,  Increment, false>( data ) ;
            default: break;
        }
    }
                    
    if( TYPEOF(data) == SYMSXP ){
      if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(data) ){
        case INTSXP:  return new Rank_Impl<INTSXP,  Increment, true>( data ) ;
        case REALSXP: return new Rank_Impl<REALSXP, Increment, true>( data ) ;
        case STRSXP:  return new Rank_Impl<STRSXP,  Increment, true>( data ) ;
        default: break;
    }
    // we don't know how to handle it.
    return 0 ;
}

Result* lead_prototype(SEXP call, const LazySubsets& subsets, int nargs){
    if( nargs != 2 ) return 0 ;
    Armor<SEXP> data( CADR(call) );
    int n = as<int>( CADDR(call) );
    if( TYPEOF(data) == SYMSXP ){
      if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(data) ){
        case INTSXP:
            if( Rf_inherits(data, "Date") ) return new TypedLead<INTSXP>(data, n, get_date_classes() ) ;
            return new Lead<INTSXP>(data, n) ;
        case REALSXP:
            if( Rf_inherits(data, "POSIXct") ) return new TypedLead<REALSXP>(data, n, get_time_classes() ) ;
            if( Rf_inherits(data, "Date") ) return new TypedLead<REALSXP>(data, n, get_date_classes() ) ;
            return new Lead<REALSXP>(data, n) ;
        case STRSXP: return new Lead<STRSXP>(data, n) ;
        case LGLSXP: return new Lead<LGLSXP>(data, n) ;
        default: break ;
    }
    return 0 ;
}

Result* lag_prototype(SEXP call, const LazySubsets& subsets, int nargs){
    if( nargs != 2 ) return 0 ;
    Armor<SEXP> data( CADR(call) );
    int n = as<int>( CADDR(call) );
    if( TYPEOF(data) == SYMSXP ){
      if( subsets.count(data) ) data = subsets.get_variable(data) ;                                       
      else return 0 ;
    }
    switch( TYPEOF(data) ){
        case INTSXP:
            if( Rf_inherits(data, "Date") ) return new TypedLag<INTSXP>(data, n, get_date_classes() ) ;
            return new Lag<INTSXP>(data, n) ;
        case REALSXP:
            if( Rf_inherits(data, "POSIXct") ) return new TypedLag<REALSXP>(data, n, get_time_classes() ) ;
            if( Rf_inherits(data, "Date") ) return new TypedLag<REALSXP>(data, n, get_date_classes() ) ;
            return new Lag<REALSXP>(data, n) ;
        case STRSXP: return new Lag<STRSXP>(data, n) ;
        case LGLSXP: return new Lag<LGLSXP>(data, n) ;
        default: break ;
    }
    return 0 ;
}  
    
template < template <int> class Templ>
Result* cumfun_prototype(SEXP call, const LazySubsets& subsets, int nargs){
    if( nargs != 1 ) return 0 ;
    Armor<SEXP> data( CADR(call) );
    if(TYPEOF(data) == SYMSXP) data = subsets.get_variable(data) ;
    switch( TYPEOF(data) ){
        case INTSXP: return new Templ<INTSXP>(data) ;
        case REALSXP: return new Templ<REALSXP>(data) ;
        default: break ;
    }
    return 0 ;
}

HybridHandlerMap& get_handlers(){
    static HybridHandlerMap handlers ;
    if( !handlers.size() ){
        handlers[ Rf_install( "n")               ] = count_prototype ;
        handlers[ Rf_install( "n_distinct" )     ] = count_distinct_prototype ;
        handlers[ Rf_install( "row_number" )     ] = row_number_prototype ;
        
        handlers[ Rf_install( "min" )            ] = minmax_prototype<dplyr::Min> ;
        handlers[ Rf_install( "max" )            ] = minmax_prototype<dplyr::Max> ;
        
        handlers[ Rf_install( "mean" )           ] = simple_prototype<dplyr::Mean> ;
        handlers[ Rf_install( "var" )            ] = simple_prototype<dplyr::Var> ;
        handlers[ Rf_install( "sd")              ] = simple_prototype<dplyr::Sd> ;
        handlers[ Rf_install( "sum" )            ] = simple_prototype<dplyr::Sum>;
        
        handlers[ Rf_install( "min_rank" )       ] = rank_impl_prototype<dplyr::internal::min_rank_increment> ;
        handlers[ Rf_install( "dense_rank" )     ] = rank_impl_prototype<dplyr::internal::dense_rank_increment> ;

        // handlers[ Rf_install( "cumsum")          ] = cumfun_prototype<CumSum> ;
        // handlers[ Rf_install( "cummin")          ] = cumfun_prototype<CumMin> ;
        // handlers[ Rf_install( "cummax")          ] = cumfun_prototype<CumMax> ;
        
        // handlers[ Rf_install( "lead" )           ] = lead_prototype ;
        // handlers[ Rf_install( "lag" )            ] = lag_prototype ;
    }
    return handlers ;
}

Result* constant_handler(SEXP constant){
    switch(TYPEOF(constant)){
    case INTSXP:
        {
            if( Rf_inherits(constant, "Date") ) return new TypedConstantResult<INTSXP>(constant, get_date_classes() ) ;
            return new ConstantResult<INTSXP>(constant) ;
        }
    case REALSXP:
        {
            if( Rf_inherits(constant, "POSIXct") ) return new TypedConstantResult<REALSXP>(constant, get_time_classes() ) ;
            if( Rf_inherits(constant, "Date") ) return new TypedConstantResult<REALSXP>(constant, get_date_classes() ) ;
            return new ConstantResult<REALSXP>(constant) ;
        }
    case STRSXP: return new ConstantResult<STRSXP>(constant) ;
    case LGLSXP: return new ConstantResult<LGLSXP>(constant) ;
    }
    return 0;
}

Result* get_handler( SEXP call, const LazySubsets& subsets, const Environment& env ){
    if( TYPEOF(call) == LANGSXP ){
        int depth = Rf_length(call) ;
        HybridHandlerMap& handlers = get_handlers() ;
        SEXP fun_symbol = CAR(call) ;
        if( TYPEOF(fun_symbol) != SYMSXP ) return 0 ;

        HybridHandlerMap::const_iterator it = handlers.find( fun_symbol ) ;
        if( it == handlers.end() ) return 0 ;

        return it->second( call, subsets, depth - 1 );
    } else if( TYPEOF(call) == SYMSXP ){
        if( !subsets.count(call) ){
            SEXP data = env.find( CHAR(PRINTNAME(call)) ) ;
            if( Rf_length(data) == 1 ) return constant_handler(data) ;
        }
    } else {
        // TODO: perhaps deal with SYMSXP separately
        if( Rf_length(call) == 1 ) return constant_handler(call) ;
    }
    return 0 ;
}

void registerHybridHandler( const char* name, HybridHandler proto){
    get_handlers()[ Rf_install(name) ] = proto ;
}

bool can_simplify( SEXP call ){
    if( TYPEOF(call) == LISTSXP ){
        bool res = can_simplify( CAR(call) ) ;
        if( res ) return true ;
        return can_simplify( CDR(call) ) ;
    }

    if( TYPEOF(call) == LANGSXP ){
        SEXP fun_symbol = CAR(call) ;
        if( TYPEOF(fun_symbol) != SYMSXP ) return false ;

        if( get_handlers().count( fun_symbol ) ) return true ;

        return can_simplify( CDR(call) ) ;
    }
    return false ;
}

template <typename Index>
DataFrame subset( DataFrame df, const Index& indices, CharacterVector columns, CharacterVector classes){
    DataFrameVisitors visitors(df, columns) ;
    return visitors.subset(indices, classes) ;
}

template <typename Index>
DataFrame subset( DataFrame x, DataFrame y, const Index& indices_x, const Index& indices_y, CharacterVector by, CharacterVector classes ){
    CharacterVector x_columns = x.names() ;
    DataFrameVisitors visitors_x(x, x_columns) ;

    CharacterVector all_y_columns = y.names() ;
    CharacterVector y_columns = setdiff( all_y_columns, by ) ;
    JoinColumnSuffixer suffixer(x_columns, y_columns, by) ;

    DataFrameVisitors visitors_y(y, y_columns) ;

    int nrows = indices_x.size() ;
    int nv_x = visitors_x.size(), nv_y = visitors_y.size() ;
    List out(nv_x+nv_y);
    CharacterVector names(nv_x+nv_y) ;
    int k=0;
    for( ; k<nv_x; k++){
       out[k] = visitors_x.get(k)->subset(indices_x) ;
       names[k] = suffixer.get( x_columns[k], ".x" ) ;
    }
    for( int i=0; i<nv_y; i++, k++){
       out[k] = visitors_y.get(i)->subset(indices_y) ;
       names[k] = suffixer.get(y_columns[i], ".y" ) ;
    }
    out.attr("class") = classes ;
    set_rownames(out, nrows) ;
    out.names() = names ;

    SEXP vars = x.attr( "vars" ) ;
    if( !Rf_isNull(vars) )
        out.attr( "vars" ) = vars ;

    return (SEXP)out ;
}

template <typename TargetContainer, typename SourceContainer>
void push_back( TargetContainer& x, const SourceContainer& y ){
    x.insert( x.end(), y.begin(), y.end() ) ;
}
template <typename Container>
void push_back( Container& x, typename Container::value_type value, int n ){
    for( int i=0; i<n; i++)
        x.push_back( value ) ;
}

void assert_all_white_list(const DataFrame& data){
    // checking variables are on the white list
    int nc = data.size() ;
    for( int i=0; i<nc; i++){
        if( !white_list(data[i]) ){
            std::stringstream ss ;
            CharacterVector names = data.names() ;
            ss << "column '" << names[i] << "' has unsupported type" ;
            stop(ss.str()) ;
        }
    }
}   

// [[Rcpp::export]]
DataFrame semi_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);

    // train the map in terms of x
    train_push_back( map, x.nrows(), x.nrows() / 10) ;

    int n_y = y.nrows() ;
    // this will collect indices from rows in x that match rows in y
    std::vector<int> indices ;
    for( int i=0; i<n_y; i++){
        // find a row in x that matches row i from y
        Map::iterator it = map.find(-i-1) ;

        if( it != map.end() ){
            // collect the indices and remove them from the
            // map so that they are only found once.
            push_back( indices, it->second ) ;

            map.erase(it) ;

        }
    }

    return subset(x, indices, x.names(), x.attr("class") ) ;
}

// [[Rcpp::export]]
DataFrame anti_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);

    // train the map in terms of x
    train_push_back( map, x.nrows(), x.nrows() / 10 ) ;

    int n_y = y.nrows() ;
    // remove the rows in x that match
    for( int i=0; i<n_y; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() )
            map.erase(it) ;
    }

    // collect what's left
    std::vector<int> indices ;
    for( Map::iterator it = map.begin() ; it != map.end(); ++it)
        push_back( indices, it->second ) ;

    return subset(x, indices, x.names(), x.attr( "class" ) ) ;
}

// [[Rcpp::export]]
DataFrame inner_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);

    // train the map in terms of x
    train_push_back( map, x.nrows(), x.nrows() / 10 ) ;

    std::vector<int> indices_x ;
    std::vector<int> indices_y ;

    int n_y = y.nrows() ;
    for( int i=0; i<n_y; i++){
        // find indices for rows in x that match the row i in y
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            push_back( indices_x, it->second );
            push_back( indices_y, i, it->second.size() ) ;
        }
    }

    return subset( x, y, indices_x, indices_y, by, x.attr( "class") );
}

// [[Rcpp::export]]
DataFrame left_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(y, x, by) ;
    Map map(visitors);

    // train the map in terms of y
    train_push_back( map, y.nrows(), y.nrows() / 10 ) ;

    std::vector<int> indices_x ;
    std::vector<int> indices_y ;

    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++){
        // find a row in y that matches row i in x
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            push_back( indices_y,    it->second ) ;
            push_back( indices_x, i, it->second.size() ) ;
        } else {
            indices_y.push_back(-1) ; // mark NA
            indices_x.push_back(i) ;
        }
    }
    return subset( x, y, indices_x, indices_y, by, x.attr( "class" ) ) ;
}

// [[Rcpp::export]]
DataFrame right_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);

    // train the map in terms of y
    train_push_back( map, x.nrows(), x.nrows() / 10 ) ;

    std::vector<int> indices_x ;
    std::vector<int> indices_y ;

    int n_y = y.nrows() ;
    for( int i=0; i<n_y; i++){
        // find a row in y that matches row i in x
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            push_back( indices_x,    it->second ) ;
            push_back( indices_y, i, it->second.size() ) ;
        } else {
            indices_x.push_back(-1) ; // mark NA
            indices_y.push_back(i) ;
        }
    }
    return subset( x, y, indices_x, indices_y, by, x.attr( "class" ) ) ;
}

SEXP promote(SEXP x){
    if( TYPEOF(x) == INTSXP ){
        IntegerVector data(x) ;
        if( Rf_inherits( x, "factor" ) ){
            CharacterVector levels = data.attr( "levels" ) ;
            int n = data.size() ;
            CharacterVector out( data.size() ) ;
            for( int i=0; i<n; i++ ){
                out[i] = levels[data[i]-1] ;
            }
            return out ;
        } else {
            return NumericVector(x) ;
        }
    }
    return x ;
}

SEXP pairlist_shallow_copy(SEXP p){
    Shield<SEXP> attr( Rf_cons(CAR(p), R_NilValue) ) ;
    SEXP q = attr ;
    SET_TAG(q, TAG(p)) ;
    p = CDR(p) ;
    while( !Rf_isNull(p) ){
        Shield<SEXP> s( Rf_cons(CAR(p), R_NilValue) ) ;
        SETCDR(q, s) ;
        q = CDR(q) ;
        SET_TAG(q, TAG(p)) ;
        p = CDR(p) ;
    }
    return attr ;   
}

void copy_attributes(SEXP out, SEXP data){
    SEXP att = ATTRIB(data) ;
    if( !Rf_isNull(att) ){
        SET_ATTRIB( out, pairlist_shallow_copy(ATTRIB(data)) ) ;
    }
    SET_OBJECT( out, OBJECT(data) );
}

// [[Rcpp::export]]
SEXP shallow_copy(const List& data){
    int n = data.size() ;
    List out(n) ;
    for( int i=0; i<n; i++) {
      out[i] = shared_SEXP(data[i]) ;
    }
    copy_attributes(out, data) ;
    return out ;
}

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame( DataFrame& x, DataFrame& y, bool ignore_col_order = true, bool convert = false ){
    int n = x.size() ;
    
    CharacterVector names_x, names_y ;
    
    bool null_x = Rf_isNull(x.names()), null_y = Rf_isNull(y.names()) ;
    if( null_x && !null_y ){
        return no_because( "x does not have names, but y does") ;        
    } else if( null_y && !null_x){
        return no_because( "y does not have names, but x does") ;
    } else if( null_x && null_y){
        names_x = CharacterVector(n) ;
        std::string v("v") ;
        for( int i=0; i<n; i++){
            std::stringstream ss ;
            ss << "v" << (i+1) ;
            names_x[i] = ss.str() ;    
        }
        x = shallow_copy(x) ;
        x.names() = names_x ;
        
        int ny = y.size() ;
        names_y = CharacterVector(ny) ;
        for( int i=0; i<ny; i++){
            std::stringstream ss ;
            ss << "v" << (i+1) ;
            names_y[i] = ss.str()  ;    
        }
        y = shallow_copy(y) ;
        y.names() = names_y ;
        
        
    } else {
        names_x = x.names() ;
        names_y = y.names() ;
    }
    
    CharacterVector names_y_not_in_x = setdiff( names_y, names_x );
    CharacterVector names_x_not_in_y = setdiff( names_x, names_y );
    std::stringstream ss ;
    bool ok = true ;

    if( !ignore_col_order ){
        if( names_y_not_in_x.size() == 0 && names_y_not_in_x.size() == 0 ){
            // so the names are the same, check if they are in the same order
            for( int i=0; i<n; i++){
                if( names_x[i] != names_y[i] ){
                    ok = false ;
                    break ;
                }
            }
            if( !ok ){
                ss <<  "Same column names, but different order" ;
                return no_because( ss.str() ) ;
            }
        }
    }

    if( names_y_not_in_x.size() ){
        ok = false ;
        ss << "Cols in y but not x: " << collapse(names_y_not_in_x) ;
    }

    if( names_x_not_in_y.size() ){
        ok = false ;
        ss << "Cols in x but not y: " << collapse(names_x_not_in_y) ;
    }

    if(!ok){
        return no_because( ss.str() ) ;
    }

    if( convert ){
        x = clone(x) ;
        y = clone(y) ;
        for( int i = 0; i<n; i++){
            x[i] = promote( x[i] ) ;
            y[i] = promote( y[i] ) ;
        }
    }

    DataFrameVisitors v_x( x, names_x );
    DataFrameVisitors v_y( y, names_x );

    ok = true ;
    for( int i=0; i<n; i++){
        if( typeid(*v_x.get(i)) != typeid(*v_y.get(i)) ){
            ss << "Incompatible type for column "
               << names_x[i]
               << ": x "
               << v_x.get(i)->get_r_type()
               << ", y "
               << v_y.get(i)->get_r_type() ;
            ok = false ;
        } else {
            String name = names_x[i];
            if( ! v_x.get(i)->is_compatible( v_y.get(i), ss, name ) ){
                ok = false ;
            }
        }

    }
    if(!ok) return no_because( ss.str() ) ;
    return yes() ;
}

class RowTrack {
public:
    RowTrack( const std::string& msg, int max_count_ = 10 ) : ss(), count(0), max_count(max_count_) {
        ss << msg ;
    }

    void record( int i){
        if( count > max_count ) return ;
        if( count ) ss << ", " ;
        int idx = i >= 0 ? (i+1) : -i ;
        ss << idx ;
        if( count == max_count ) ss << "[...]" ;
        count++ ;
    }

    bool empty() const {
        return count == 0 ;
    }

    std::string str() const {
        return ss.str() ;
    }

private:
    std::stringstream ss ;
    int count ;
    int max_count ;
} ;

// [[Rcpp::export]]
dplyr::BoolResult equal_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = true, bool ignore_row_order = true, bool convert = false ){
    BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
    if( !compat ) return compat ;

    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, x.names() ) ;
    Map map(visitors);

    // train the map in both x and y
    int nrows_x = x.nrows() ;
    for( int i=0; i<nrows_x; i++) map[i].push_back(i) ;

    int nrows_y = y.nrows() ;
    for( int i=0; i<nrows_y; i++) map[-i-1].push_back(-i-1) ;

    RowTrack track_x( "Rows in x but not y: " ) ;
    RowTrack track_y( "Rows in y but not x: " ) ;

    bool ok = true ;
    Map::const_iterator it = map.begin() ;

    for( ; it != map.end(); ++it){
        // retrieve the indices ( -ves for y, +ves for x )
        const std::vector<int>& chunk = it->second ;
        int n = chunk.size() ;

        int count_left = 0, count_right = 0 ;
        for( int i=0; i<n; i++){
            if( chunk[i] < 0 )
                count_right++ ;
            else
                count_left++ ;
        }
        if( count_right == 0 ){
            track_x.record( chunk[0] ) ;
            ok = false ;
        }
        if( count_left == 0){
            track_y.record( chunk[0] ) ;
            ok = false ;
        }

    }

    if(!ok){
        std::stringstream ss ;
        if( ! track_x.empty() ) ss << track_x.str() ;
        if( ! track_y.empty() ) ss << track_y.str() ;
        return no_because( ss.str() ) ;
    }

    if(ok && ignore_row_order) return yes();

    if( !ignore_row_order ){
        if( nrows_x != nrows_y )
            return no_because( "Different number of rows" ) ;
        for( int i=0; i<nrows_x; i++){
            if( !visitors.equal( i, -i-1) ){
                    return no_because( "Same row values, but different order" ) ;
            }
        }
    }

    return yes() ;
}

// [[Rcpp::export]]
dplyr::BoolResult all_equal_data_frame( List args, Environment env ){
    int n = args.size() ;
    DataFrame x0 = Rf_eval( args[0], env) ;
    for( int i=1; i<n; i++){
        BoolResult test = equal_data_frame( x0, Rf_eval( args[i], env ) ) ;
        if( !test ) return test ;
    }
    return yes() ;
}

// [[Rcpp::export]]
DataFrame union_data_frame( DataFrame x, DataFrame y){
    if( !compatible_data_frame(x,y) )
        stop( "not compatible" );

    typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
    DataFrameJoinVisitors visitors(x, y, x.names() ) ;
    Set set(visitors);

    train_insert( set, x.nrows() ) ;
    train_insert_right( set, y.nrows() ) ;

    return visitors.subset( set, x.attr("class") ) ;
}

// [[Rcpp::export]]
DataFrame intersect_data_frame( DataFrame x, DataFrame y){
    if( !compatible_data_frame(x,y) )
        stop( "not compatible" );

    typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
    DataFrameJoinVisitors visitors(x, y, x.names() ) ;
    Set set(visitors);

    train_insert( set, x.nrows() ) ;

    std::vector<int> indices ;
    int n_y = y.nrows() ;
    for( int i=0; i<n_y; i++) {
        Set::iterator it = set.find( -i-1 ) ;
        if( it != set.end() ){
            indices.push_back(*it) ;
            set.erase(it) ;
        }
    }

    return visitors.subset( indices, x.attr("class") ) ;
}

// [[Rcpp::export]]
DataFrame setdiff_data_frame( DataFrame x, DataFrame y){
    if( !compatible_data_frame(x,y) )
        stop( "not compatible" );

    typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
    DataFrameJoinVisitors visitors(y, x, y.names() ) ;
    Set set(visitors);

    train_insert( set, y.nrows() ) ;

    std::vector<int> indices ;

    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++) {
        if( !set.count(-i-1) ){
            set.insert(-i-1) ;
            indices.push_back(-i-1) ;
        }
    }

    return visitors.subset( indices, x.attr("class") ) ;
}

// [[Rcpp::export]]
IntegerVector match_data_frame( DataFrame x, DataFrame y){
    if( !compatible_data_frame(x,y) )
        stop( "not compatible" );

    typedef VisitorSetIndexSet<DataFrameJoinVisitors> Set ;
    DataFrameJoinVisitors visitors(y, x, x.names() ) ;
    Set set(visitors);

    train_insert( set, y.nrows() ) ;

    int n_x = x.nrows() ;
    IntegerVector res = no_init( n_x );
    for( int i=0; i<n_x; i++) {
        Set::iterator it = set.find( -i-1 );
        res[i] = ( it == set.end() ) ? NA_INTEGER : (*it+1) ;
    }

    return res ;
}

// [[Rcpp::export]]
DataFrame grouped_df_impl( DataFrame data, ListOf<Symbol> symbols, bool drop ){
    assert_all_white_list(data); 
    DataFrame copy = shallow_copy(data) ;
    copy.attr("vars") = symbols ;
    copy.attr("drop") = drop ;
    return build_index_cpp(copy) ;
}

DataFrame build_index_cpp( DataFrame data ){
    ListOf<Symbol> symbols( data.attr( "vars" ) ) ;

    int nsymbols = symbols.size() ;
    CharacterVector vars(nsymbols) ;
    for( int i=0; i<nsymbols; i++){
        vars[i] = PRINTNAME(symbols[i]) ;
        
        const char* name = vars[i] ;
        SEXP v = data[name] ;
        if( !white_list(v) || TYPEOF(v) == VECSXP ){
            std::stringstream ss ;
            ss << "cannot group column " 
               << name 
               <<", of class '"
               << get_single_class(v) 
               << "'" ;
            stop(ss.str()) ;
        }
    }

    DataFrameVisitors visitors(data, vars) ;
    ChunkIndexMap map( visitors ) ;

    // checking 10 times for interupts
    train_push_back( map, data.nrows(), data.nrows() / 10 ) ;

    DataFrame labels = visitors.subset( map, "data.frame") ;
    int ngroups = labels.nrows() ;

    OrderVisitors labels_order_visitors(labels) ;
    IntegerVector labels_order = labels_order_visitors.apply() ;
    
    DataFrameVisitors labels_visitors(labels ) ;
    labels = labels_visitors.subset(labels_order, "data.frame" ) ;
    
    List indices(ngroups) ;
    IntegerVector group_sizes = no_init( ngroups );
    int biggest_group = 0 ;

    ChunkIndexMap::const_iterator it = map.begin() ;
    std::vector<const std::vector<int>* > chunks(ngroups) ;
    for( int i=0; i<ngroups; i++, ++it){
        chunks[i] = &it->second ;    
    }
    for( int i=0; i<ngroups; i++){
        int idx = labels_order[i] ; 
        const std::vector<int>& chunk = *chunks[idx] ;
        indices[i] = chunk ;
        group_sizes[i] = chunk.size() ;
        biggest_group = std::max( biggest_group, (int)chunk.size() );
    }

    data.attr( "indices" ) = indices ;
    data.attr( "group_sizes") = group_sizes ;
    data.attr( "biggest_group_size" ) = biggest_group ;
    data.attr( "labels" ) = labels ;
    data.attr( "class" ) = CharacterVector::create("grouped_df", "tbl_df", "tbl", "data.frame") ;
    return data ;
}

DataFrame build_index_adj(DataFrame df, ListOf<Symbol> symbols ){
    int nsymbols = symbols.size() ;
    CharacterVector vars(nsymbols) ;
    for( int i=0; i<nsymbols; i++){
        vars[i] = PRINTNAME(symbols[i]) ;
    }

    DataFrameVisitors visitors(df, vars) ;
    std::vector<int> sizes ;
    int n = df.nrows() ;
    
    int i=0 ;
    while( i<n ){
        int start = i++ ;
        for( ; i<n && visitors.equal(i, start) ; i++) ;
        sizes.push_back(i-start) ;
    }
    
    n = sizes.size() ;
    List indices(n);
    IntegerVector first = no_init(n) ;
    int start = 0 ;
    int biggest_group = 0 ;
    for( int i=0; i<n; i++){
        first[i] = start ;
        int end = start + sizes[i] - 1 ;
        indices[i] = seq(start, end) ;
        start = end + 1 ;
        biggest_group = std::max( biggest_group, sizes[i]) ;
    }
    
    df.attr( "indices") = indices ;
    df.attr( "labels")  = visitors.subset(first, "data.frame") ;
    df.attr( "group_sizes") = sizes ;
    df.attr( "biggest_group_size") = biggest_group ;
    df.attr( "class" ) = CharacterVector::create("adj_grouped_df", "grouped_df", "tbl_df", "tbl", "data.frame") ;
    df.attr( "vars" ) = symbols ;
    
    return df ;
}

// [[Rcpp::export]]
DataFrame grouped_df_adj_impl( DataFrame data, ListOf<Symbol> symbols, bool drop ){
    DataFrame copy = shallow_copy(data) ;
    copy.attr("vars") = symbols ;
    copy.attr("drop") = drop ;
    return build_index_adj(data, symbols) ;
}

typedef dplyr_hash_set<SEXP> SymbolSet ;

inline SEXP check_filter_integer_result(SEXP tmp){
    if( TYPEOF(tmp) != INTSXP ){
        stop( "integer_filter condition does not evaluate to an integer vector. " ) ;
    }
    return tmp ;
}

SEXP integer_filter_grouped(GroupedDataFrame gdf, const List& args, const DataDots& dots){
    const DataFrame& data = gdf.data() ;
    Environment env = dots.envir(0);
    CharacterVector names = data.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }
    
    // we already checked that we have only one expression
    Call call( (SEXP)args[0] ) ;
    
    std::vector<int> indx ; indx.reserve(1000) ;
    
    IntegerVector g_test ;
    GroupedCallProxy call_proxy( call, gdf, env ) ;
    
    int ngroups = gdf.ngroups() ;
    GroupedDataFrame::group_iterator git = gdf.group_begin() ;
    for( int i=0; i<ngroups; i++, ++git){
        SlicingIndex indices = *git ;
        g_test = check_filter_integer_result( call_proxy.get( indices ) ) ;
        
        int ntest = g_test.size() ;
        for( int j=0; j<ntest; j++){
            int k = g_test[j] ;
            if( k > 0 && k <= indices.size() ){
                indx.push_back( indices[k-1] ) ;
            }
        }
        
    }
    
    DataFrame res = subset( data, indx, names, classes_grouped() ) ;
    res.attr( "vars")   = data.attr("vars") ;
    
    return res ;

}

SEXP integer_filter_not_grouped( const DataFrame& df, const List& args, const DataDots& dots){
    CharacterVector names = df.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }
    
    Environment env = dots.envir(0) ;
    Call call( (SEXP)args[0] );
    CallProxy proxy( call, df, env ) ;
    
    IntegerVector test = check_filter_integer_result(proxy.eval()) ;
    // this should rather be handled in subset
    test = test - 1 ;
    DataFrame res = subset( df, test, df.names(), classes_not_grouped() ) ;
    return res ;
    
}

// [[Rcpp::export]]
SEXP integer_filter_impl( DataFrame df, List args, Environment env){
    if( args.size() != 1 )
        stop( "integer_filter only accepts one expression" );
    DataDots dots(env) ;
    if( is<GroupedDataFrame>(df) ){
        return integer_filter_grouped( GroupedDataFrame(df), args, dots ) ;
    } else {
        return integer_filter_not_grouped(df, args, dots ) ;
    }
}

SEXP structure_mutate( const NamedListAccumulator& accumulator, const DataFrame& df, CharacterVector classes){
    List res = accumulator ;
    res.attr("class") = classes ;
    set_rownames( res, df.nrows() ) ;
    res.attr( "vars")     = df.attr("vars") ;
    res.attr( "labels" )  = df.attr("labels" );
    res.attr( "index")    = df.attr("index") ;
    res.attr( "indices" ) = df.attr("indices" ) ;

    return res ;
}

void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf){
    int n = result_names.size() ;
    for( int i=0; i<n; i++){
        if( gdf.has_group( result_names[i] ) )
            stop( "cannot modify grouping variable" ) ;
    }
}

SEXP mutate_grouped(GroupedDataFrame gdf, List args, const DataDots& dots){
    const DataFrame& df = gdf.data() ;
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    check_not_groups(results_names, gdf);

    Environment env = dots.envir(0) ;
    GroupedCallProxy proxy(gdf, env) ;
    Shelter<SEXP> __ ;

    NamedListAccumulator accumulator ;
    int ncolumns = df.size() ;
    CharacterVector column_names = df.names() ;
    for( int i=0; i<ncolumns; i++){
        accumulator.set( column_names[i], df[i] ) ;
    }

    for( int i=0; i<nexpr; i++){
        env = dots.envir(i) ;
        proxy.set_env( env ) ;
        SEXP call = args[i] ;
        SEXP name = results_names[i] ;
        SEXP variable = R_NilValue ;
        if( TYPEOF(call) == SYMSXP ){
            if(proxy.has_variable(call)){
                variable = proxy.get_variable( PRINTNAME(call) ) ;
            } else {
                SEXP v = env.find(CHAR(PRINTNAME(call))) ;
                if( Rf_isNull(v) ){
                    std::stringstream s ;
                    s << "unknown variable: " << CHAR(PRINTNAME(call)) ;
                    stop(s.str());
                } else if( Rf_length(v) == 1){
                    Replicator* rep = constant_replicator(v, gdf.nrows() );
                    variable = __( rep->collect() );
                    delete rep ;
                } else {
                    Replicator* rep = replicator(v, gdf) ;
                    variable = __( rep->collect() );
                    delete rep ;
                }
            }

        } else if(TYPEOF(call) == LANGSXP){
            proxy.set_call( call );
            Gatherer* gather = gatherer( proxy, gdf, name ) ;
            variable = __( gather->collect() ) ;
            delete gather ;
        } else if(Rf_length(call) == 1) {
            boost::scoped_ptr<Gatherer> gather( constant_gatherer( call, gdf.nrows() ) );
            variable = __( gather->collect() ) ;
        } else {
            stop( "cannot handle" ) ;
        }

        proxy.input( name, variable ) ;
        accumulator.set( name, variable) ;
    }

    return structure_mutate(accumulator, df, classes_grouped() );
}

SEXP mutate_not_grouped(DataFrame df, List args, const DataDots& dots){
    Shelter<SEXP> __ ;

    Environment env = dots.envir(0) ;

    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;

    NamedListAccumulator accumulator ;
    int nvars = df.size() ;
    CharacterVector df_names = df.names() ;
    for( int i=0; i<nvars; i++){
        accumulator.set( df_names[i], df[i] ) ;
    }

    CallProxy call_proxy(df, env) ;
    for( int i=0; i<nexpr; i++){
        env = dots.envir(i) ;
        call_proxy.set_env(env) ;

        SEXP call = args[i] ;
        SEXP name = results_names[i] ;
        SEXP result = R_NilValue ;
        if( TYPEOF(call) == SYMSXP ){
            if(call_proxy.has_variable(call)){
                result = call_proxy.get_variable(PRINTNAME(call)) ;
            } else {
                result = shared_SEXP(env.find(CHAR(PRINTNAME(call)))) ;
            }
        } else if( TYPEOF(call) == LANGSXP ){
            call_proxy.set_call( args[i] );

            // we need to protect the SEXP, that's what the Shelter does
            result = __( call_proxy.eval() ) ;

        } else if( Rf_length(call) == 1 ){
            boost::scoped_ptr<Gatherer> gather( constant_gatherer( call, df.nrows() ) );
            result = __( gather->collect() ) ;
        } else {
            stop( "cannot handle" ) ;
        }

        check_supported_type(result, name) ;
        
        if( Rf_length(result) == df.nrows() ){
            // ok
        } else if( Rf_length(result) == 1 ){
            // recycle
            Gatherer* gather = constant_gatherer( result, df.nrows() ) ;
            result = __( gather->collect() ) ;
            delete gather ;
        } else {
            std::stringstream s ;
            s << "wrong result size ("
              << Rf_length(result)
              << "), expected "
              << df.nrows()
              << " or 1" ;
            stop(s.str()) ;
        }

        call_proxy.input( name, result ) ;
        accumulator.set( name, result );
    }

    List res = structure_mutate(accumulator, df, classes_not_grouped() ) ;

    return res ;
}


// [[Rcpp::export]]
SEXP mutate_impl( DataFrame df, List args, Environment env){
    DataDots dots(env) ;
    if( is<GroupedDataFrame>( df ) ){
        return mutate_grouped( GroupedDataFrame(df), args, dots);
    } else {
        return mutate_not_grouped( df, args, dots) ;
    }
}

// [[Rcpp::export]]
IntegerVector order_impl( List args, Environment env ){
    int nargs = args.size() ;
    SEXP tmp ;
    List variables(nargs) ;
    LogicalVector ascending(nargs) ;
    for(int i=0; i<nargs; i++){
        tmp = args[i] ;
        if( TYPEOF(tmp) == LANGSXP && CAR(tmp) == Rf_install("desc") ){
            variables[i] = Rf_eval( CAR(CDR(tmp) ), env ) ;
            ascending[i] = false ;
        } else{
            variables[i] = Rf_eval( tmp, env );
            ascending[i] = true ;
        }
    }
    OrderVisitors o(variables,ascending, nargs) ;
    IntegerVector res = o.apply() ;
    res = res + 1 ;
    return res ;
}

// [[Rcpp::export]]
List arrange_impl( DataFrame data, List args, DataDots dots ){
    assert_all_white_list(data) ;
    
    int nargs = args.size() ;
    List variables(nargs) ;
    LogicalVector ascending(nargs) ;
    Shelter<SEXP> __ ;
    
    for(int i=0; i<nargs; i++){
        SEXP call = args[i] ;
        bool is_desc = TYPEOF(call) == LANGSXP && Rf_install("desc") == CAR(call) ;
        
        CallProxy call_proxy(is_desc ? CADR(call) : call, data, dots.envir(i)) ;
        
        SEXP v = __(call_proxy.eval()) ;
        if( !white_list(v) || TYPEOF(v) == VECSXP ){
            std::stringstream ss ;
            ss << "cannot arrange column of class '"
               << get_single_class(v) 
               << "'" ;
            stop(ss.str()) ;
        }
        
        if( Rf_length(v) != data.nrows() ){
            std::stringstream s ;
            s << "incorrect size ("
              << Rf_length(v)
              << "), expecting :"
              << data.nrows() ;
            stop(s.str()) ;
        }
        variables[i] = v ;
        ascending[i] = !is_desc ;
    }
    OrderVisitors o(variables, ascending, nargs) ;
    IntegerVector index = o.apply() ;
    
    DataFrameVisitors visitors( data, data.names() ) ;
    List res = visitors.subset(index, data.attr("class") ) ;
    return res;
}

// [[Rcpp::export]]
DataFrame sort_impl( DataFrame data ){
    OrderVisitors o(data) ;
    IntegerVector index = o.apply() ;

    DataFrameVisitors visitors( data, data.names() ) ;
    DataFrame res = visitors.subset(index, "data.frame" ) ;
    return res;
}

// [[Rcpp::export]]
IntegerVector group_size_grouped_cpp( GroupedDataFrame gdf ){
    return Count().process(gdf) ;
}

SEXP summarise_grouped(const GroupedDataFrame& gdf, List args, const DataDots& dots){
    DataFrame df = gdf.data() ;

    int nexpr = args.size() ;
    int nvars = gdf.nvars() ;
    CharacterVector results_names = args.names() ;
    check_not_groups(results_names, gdf);
    NamedListAccumulator accumulator ;

    int i=0;
    for( ; i<nvars; i++){
        accumulator.set( PRINTNAME(gdf.symbol(i)), shared_SEXP(gdf.label(i)) ) ;
    }

    LazyGroupedSubsets subsets(gdf) ;
    Shelter<SEXP> __ ;
    for( int k=0; k<nexpr; k++, i++ ){
        Environment env = dots.envir(k) ;

        Result* res = get_handler( args[k], subsets, env ) ;
        
        // if we could not find a direct Result
        // we can use a GroupedCalledReducer which will callback to R
        if( !res ) res = new GroupedCalledReducer( args[k], subsets, env) ;
        
        
        SEXP result = __( res->process(gdf) ) ;
        SEXP name = results_names[k] ;
        accumulator.set( name, result );
        subsets.input( Symbol(name), SummarisedVariable(result) ) ;
        delete res;
    }

    return summarised_grouped_tbl_cpp(accumulator, gdf );
}

SEXP summarise_not_grouped(DataFrame df, List args, const DataDots& dots){
    int nexpr = args.size() ;
    CharacterVector names = args.names();

    LazySubsets subsets( df ) ;
    std::vector<SEXP> results ;
    std::vector<SEXP> result_names ;
    NamedListAccumulator accumulator ;

    Rcpp::Shelter<SEXP> __ ;
    for( int i=0; i<nexpr; i++){
        SEXP name = names[i] ;
        Environment env = dots.envir(i) ;
        Result* res = get_handler( args[i], subsets, env ) ;
        SEXP result ;
        if(res) {
            result = __(res->process( FullDataFrame(df) )) ;
        } else {
            result = __(CallProxy( args[i], subsets, env).eval()) ;
        }
        delete res ;
        subsets.input( Symbol(name), result ) ;
        accumulator.set(name, result);
    }

    return tbl_cpp( accumulator, 1 ) ;
}

// [[Rcpp::export]]
SEXP summarise_impl( DataFrame df, List args, Environment env){
    DataDots dots(env) ;
    if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped( GroupedDataFrame(df), args, dots);
    } else {
        return summarise_not_grouped( df, args, dots) ;
    }
}

SEXP select_not_grouped( const DataFrame& df, const CharacterVector& keep, CharacterVector new_names ){
  CharacterVector names = df.names() ;
  IntegerVector positions = match( keep, names ); 
  int n = keep.size() ; 
  List res(n) ;
  for( int i=0; i<n; i++){
    res[i] = df[ positions[i]-1 ] ;  
  }
  copy_attributes(res, df) ;
  res.names() = new_names ; 
  return res ; 
}

DataFrame select_grouped( GroupedDataFrame gdf, const CharacterVector& keep, CharacterVector new_names ){
  int n = keep.size() ;
  DataFrame copy = select_not_grouped( gdf.data(), keep, new_names );
  
  // handle vars  attribute : make a shallow copy of the list and alter 
  //   its names attribute
  List vars = shallow_copy( copy.attr("vars") ); 
  int nv = vars.size() ;
  for( int i=0; i<nv; i++){
    SEXP s = PRINTNAME(vars[i]) ;
    int j = 0; 
    for( ; j < n; j++){
      if( s == keep[j] ){
        vars = Rf_install( CHAR(new_names[j]) );  
      }
    }
  }
  copy.attr("vars") = vars ;
  
  // hangle labels attribute
  //   make a shallow copy of the data frame and alter its names attributes
  if( !Rf_isNull( copy.attr("labels" ) ) ){   
    DataFrame original_labels( copy.attr("labels" ) ) ;
    
    DataFrame labels = shallow_copy(original_labels) ;
    CharacterVector label_names = clone<CharacterVector>( labels.names() ) ;
    
    IntegerVector positions = match( label_names, keep ); 
    int nl = label_names.size() ;
    for( int i=0; i<nl; i++){
      label_names[positions[i]-1] = new_names[i] ;
    }
    labels.names() = label_names ;
    labels.attr("vars") = vars ;
    copy.attr("labels") = labels ;
  }
  
  return copy ;
}

// [[Rcpp::export]]
DataFrame select_impl( DataFrame df, CharacterVector vars ){
  if( is<GroupedDataFrame>(df) ){
    return select_grouped( GroupedDataFrame(df), vars, vars.names() ) ;  
  } else {
    return select_not_grouped(df, vars, vars.names() ) ;  
  }
}

//' Efficiently count the number of unique values in a vector.
//'
//' This is a faster and more concise equivalent of \code{length(unique(x))}
//'
//' @param x a vector of values
//' @export
//' @examples
//' x <- sample(1:10, 1e5, rep = TRUE)
//' length(unique(x))
//' n_distinct(x)
// [[Rcpp::export]]
SEXP n_distinct(SEXP x){
    SlicingIndex everything(0, Rf_length(x) );
    boost::scoped_ptr<Result> res( count_distinct_result(x) );
    if( !res ){
        std::stringstream ss ;
        ss << "cannot handle object of type" << type2name(x) ;
        stop( ss.str() ) ;
    }
    return res->process(everything) ;
}

template <typename Dots>
List rbind__impl( Dots dots ){
    int ndata = dots.size() ;
    int n = 0 ;
    for( int i=0; i<ndata; i++) n += dots[i].nrows() ;

    std::vector<Collecter*> columns ;
    std::vector<String> names ;
    int k=0 ;
    for( int i=0; i<ndata; i++){
        DataFrame df = dots[i] ;
        if( ! Rf_length(df[0]) ) continue ;
            
        DataFrameVisitors visitors( df, df.names() ) ;
        int nrows = df.nrows() ;

        CharacterVector df_names = df.names() ;
        for( int j=0; j<df.size(); j++){
            SEXP source = df[j] ;
            String name = df_names[j] ;

            Collecter* coll = 0;
            size_t index = 0 ;
            for( ; index < names.size(); index++){
                if( name == names[index] ){
                    coll = columns[index] ;
                    break ;
                }
            }
            if( ! coll ){
                coll = collecter( source, n ) ;
                columns.push_back( coll );
                names.push_back(name) ;
            }

            if( coll->compatible(source) ){
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

            } else {
                std::stringstream msg ;
                std::string column_name(name) ;
                msg << "incompatible type ("
                    << "data index: "
                    << (i+1)
                    << ", column: '"
                    << column_name
                    << "', was collecting: "
                    << coll->describe()
                    << " ("
                    << DEMANGLE(*coll)
                    << ")"
                    << ", incompatible with data of type: "
                    << type2name(source) ;

                stop( msg.str() ) ;
            }

        }

        k += nrows ;
    }

    int nc = columns.size() ;
    List out(nc) ;
    CharacterVector out_names(nc) ;
    for( int i=0; i<nc; i++){
        out[i] = columns[i]->get() ;
        out_names[i] = names[i] ;
    }
    out.attr( "names" ) = out_names ;
    delete_all( columns ) ;
    set_rownames( out, n );
    out.attr( "class" ) = "data.frame" ;

    return out ;
}

//' @export
//' @rdname rbind
// [[Rcpp::export]]
List rbind_all( ListOf<DataFrame> dots ){
    return rbind__impl(dots) ;
}

// [[Rcpp::export]]
List rbind_list__impl( DotsOf<DataFrame> dots ){
    return rbind__impl(dots) ;
}

template <typename Dots>
List cbind__impl( Dots dots ){
  int n = dots.size() ;
  
  // first check that the number of rows is the same
  int nrows = dots[0].nrows() ;
  int nv = dots[0].size() ;
  for( int i=1; i<n; i++){
    if( dots[i].nrows() != nrows ){
      std::stringstream ss ;
      ss << "incompatible number of rows (" 
         << dots[i].size()
         << ", expecting "
         << nrows 
      ;
      stop( ss.str() ) ;
    }
    nv += dots[i].size() ;
  }
  
  // collect columns
  List out(nv) ;
  CharacterVector out_names(nv) ;
  
  // then do the subsequent dfs
  for( int i=0, k=0 ; i<n; i++){
      DataFrame current = dots[i] ;
      CharacterVector current_names = current.names() ;
      int nc = current.size() ;
      for( int j=0; j<nc; j++, k++){
          out[k] = shared_SEXP(current[j]) ;
          out_names[k] = current_names[j] ;
      }
  }
  out.names() = out_names ;
  set_rownames( out, nrows ) ;
  out.attr( "class") = "data.frame" ;
  return out ;
}

// [[Rcpp::export]]
List cbind_list__impl( DotsOf<DataFrame> dots ){
  return cbind__impl( dots ) ;  
}

// [[Rcpp::export]]
List cbind_all( ListOf<DataFrame> dots ){
  return cbind__impl( dots ) ;  
}

SEXP strip_group_attributes(DataFrame df){
  Shield<SEXP> attribs( Rf_cons( classes_not_grouped(), R_NilValue ) ) ;
  SET_TAG(attribs, Rf_install("class") ) ;

  SEXP p = ATTRIB(df) ;
  std::vector<SEXP> black_list(8) ;
  black_list[0] = Rf_install("indices") ;
  black_list[1] = Rf_install("vars") ;
  black_list[2] = Rf_install("index") ;
  black_list[3] = Rf_install("labels") ;
  black_list[4] = Rf_install("drop") ;
  black_list[5] = Rf_install("group_sizes") ;
  black_list[6] = Rf_install("biggest_group_size") ;
  black_list[7] = Rf_install("class") ;

  SEXP q = attribs ;
  while( ! Rf_isNull(p) ){
    SEXP tag = TAG(p) ;
    if( std::find( black_list.begin(), black_list.end(), tag ) == black_list.end() ){
      Shield<SEXP> s( Rf_cons( CAR(p), R_NilValue) ) ;
      SETCDR(q,s) ;
      q = CDR(q) ;
      SET_TAG(q, tag) ;
    }

    p = CDR(p) ;
  }
  return attribs ;
}

// [[Rcpp::export]]
DataFrame as_regular_df(DataFrame df){
  DataFrame copy = shallow_copy(df) ;
  SET_ATTRIB(copy, strip_group_attributes(df)) ;
  SET_OBJECT(copy, OBJECT(df)) ;
  copy.attr("class") = CharacterVector::create("data.frame") ;
  return copy ;
}

// [[Rcpp::export]]
DataFrame ungroup_grouped_df( DataFrame df){
  DataFrame copy = shallow_copy(df) ;
  SET_ATTRIB(copy, strip_group_attributes(df)) ;
  return copy ;
}

// [[Rcpp::export]]
DataFrame tbl_df_impl( DataFrame df){
  return ungroup_grouped_df(df);
}

// [[Rcpp::export]]
std::vector<std::vector<int> > split_indices(IntegerVector group, int groups) {
  std::vector<std::vector<int> > ids(groups);

  int n = group.size();
  for (int i = 0; i < n; ++i) {
    ids[group[i] - 1].push_back(i + 1);
  }

  return ids;
}

