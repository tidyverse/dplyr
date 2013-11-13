#define COMPILING_DPLYR
#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef boost::unordered_map<SEXP,HybridHandler> HybridHandlerMap ;
  
#define MAKE_PROTOTYPE(__FUN__,__CLASS__)                                        \
Result* __FUN__##_prototype( SEXP call, const LazySubsets& subsets, int nargs ){ \
    if( nargs != 1 ) return 0 ;                                                  \
    SEXP arg = CADR(call) ;                                                      \
    if( TYPEOF(arg) == SYMSXP ) arg = subsets.get_variable(arg) ;                \
    switch( TYPEOF(arg) ){                                                       \
        case INTSXP:  return new dplyr::__CLASS__<INTSXP,false>( arg ) ;         \
        case REALSXP: return new dplyr::__CLASS__<REALSXP,false>( arg ) ;        \
        default: break ;                                                         \
    }                                                                            \
    return 0 ;                                                                   \
}
MAKE_PROTOTYPE(mean, Mean)
MAKE_PROTOTYPE(min, Min)
MAKE_PROTOTYPE(max, Max)
MAKE_PROTOTYPE(var, Var)
MAKE_PROTOTYPE(sd, Sd)
MAKE_PROTOTYPE(sum, Sum)

Result* count_distinct_result(SEXP vec){ 
    switch( TYPEOF(vec) ){
        case INTSXP: 
            if( Rf_inherits(vec, "factor" ))
                return new Count_Distinct<FactorVisitor>( FactorVisitor(vec) ) ;
            return new Count_Distinct< VectorVisitorImpl<INTSXP> >( VectorVisitorImpl<INTSXP>(vec) ) ;
        case REALSXP:
            if( Rf_inherits( vec, "Date" ) )
                return new Count_Distinct<DateVisitor>( DateVisitor(vec) ) ;
            if( Rf_inherits( vec, "POSIXct" ) )
                return new Count_Distinct<POSIXctVisitor>( POSIXctVisitor(vec) ) ;
            return new Count_Distinct< VectorVisitorImpl<REALSXP> >( VectorVisitorImpl<REALSXP>(vec) ) ;
        case LGLSXP:  return new Count_Distinct< VectorVisitorImpl<LGLSXP> >( VectorVisitorImpl<LGLSXP>(vec) ) ;
        case STRSXP:  return new Count_Distinct< VectorVisitorImpl<STRSXP> >( VectorVisitorImpl<STRSXP>(vec) ) ;
        default: break ;
    }
    return 0 ;
}

Result* count_prototype(SEXP, const LazySubsets&, int){
    return new Count ;
}

Result* count_distinct_prototype(SEXP call, const LazySubsets& subsets, int){
    SEXP arg = CADR(call) ;
    if( TYPEOF(arg) == SYMSXP ) return count_distinct_result( subsets.get_variable(arg) ) ;
    return count_distinct_result( arg ) ;
}

HybridHandlerMap& get_handlers(){
    static HybridHandlerMap handlers ;
    if( !handlers.size() ){
        handlers[ Rf_install("n")                ] = count_prototype ;
        handlers[ Rf_install( "mean" )           ] = mean_prototype ;
        handlers[ Rf_install( "min" )            ] = min_prototype ;
        handlers[ Rf_install( "max" )            ] = max_prototype ;
        handlers[ Rf_install( "var" )            ] = var_prototype ;
        handlers[ Rf_install( "sd")              ] = sd_prototype ;
        handlers[ Rf_install( "sum" )            ] = sum_prototype;
        handlers[ Rf_install( "count_distinct" ) ] = count_distinct_prototype ;
    }
    return handlers ;    
}

Result* get_handler( SEXP call, const LazySubsets& subsets ){
    int depth = Rf_length(call) ;
    HybridHandlerMap& handlers = get_handlers() ;
    SEXP fun_symbol = CAR(call) ;
    if( TYPEOF(fun_symbol) != SYMSXP ) return 0 ;
    
    HybridHandlerMap::const_iterator it = handlers.find( fun_symbol ) ;
    if( it == handlers.end() ) return 0 ;
    
    return it->second( call, subsets, depth - 1 );
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
        int depth = Rf_length( call ) ;
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
    DataFrameVisitors visitors_y(y, y_columns) ;
    
    int nrows = indices_x.size() ;
    int nv_x = visitors_x.size(), nv_y = visitors_y.size() ;
    List out(nv_x+nv_y);
    CharacterVector names(nv_x+nv_y) ;
    int k=0;
    for( ; k<nv_x; k++){
       out[k] = visitors_x.get(k)->subset(indices_x) ;
       names[k] = x_columns[k] ;
    }
    for( int i=0; i<nv_y; i++, k++){
       out[k] = visitors_y.get(i)->subset(indices_y) ; 
       names[k] = y_columns[i] ;
    }
    out.attr("class") = classes ;
    set_rownames(out, nrows) ;
    out.names() = names ;
    
    SEXP vars = x.attr( "vars" ) ;
    if( !Rf_isNull(vars) )
        out.attr( "vars" ) = vars ;
            
    return out ;
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

// [[Rcpp::export]]
DataFrame semi_join_impl( DataFrame x, DataFrame y, CharacterVector by){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);  
    
    // train the map in terms of x
    train_push_back( map, x.nrows() ) ;
    
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
    train_push_back( map, x.nrows() ) ;
    
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
    train_push_back( map, x.nrows() ) ;
    
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
    train_push_back( map, y.nrows() ) ;
    
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
    train_push_back( map, x.nrows() ) ;
    
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

// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame( DataFrame& x, DataFrame& y, bool ignore_col_order = true, bool convert = false ){
    int n = x.size() ;
    
    CharacterVector names_x = x.names() ; 
    CharacterVector names_y = y.names() ; 
    
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
        ss << "columns in y not in x: " << collapse(names_y_not_in_x) ;   
    }
    
    if( names_x_not_in_y.size() ){
        ok = false ;
        ss << "columns in x not in y: " << collapse(names_x_not_in_y) ;   
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
        
    RowTrack track_x( "Rows in x, but not in y: " ) ;
    RowTrack track_y( "Rows in y, but not in x: " ) ;
    
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
    
    if(ok) return yes(); 
    
    if( !ignore_row_order ){
        if( nrows_x != nrows_y )
            return no_because( "Different number of rows" ) ;
        for( int i=0; i<nrows_x; i++)
            if( !visitors.equal( i, -i-1) )
                return no_because( "Same row values, but different order" ) ;
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
DataFrame build_index_cpp( DataFrame data ){
    CharacterVector vars = Rf_getAttrib( data.attr( "vars" ), R_NamesSymbol ) ;
    
    DataFrameVisitors visitors(data, vars) ;
    ChunkIndexMap map( visitors ) ;
    train_push_back( map, data.nrows() ) ;
    
    DataFrame labels = visitors.subset( map, "data.frame") ;
    int ngroups = labels.nrows() ;
    
    OrderVisitors order_labels( labels, vars ) ;
    IntegerVector orders = order_labels.apply() ;
    
    std::vector< const std::vector<int>* > chunks(ngroups) ;
    ChunkIndexMap::const_iterator it = map.begin() ;
    for( int i=0; i<ngroups; i++, ++it){
        chunks[ i ] = &it->second ;
    }
    IntegerVector group_sizes = no_init( ngroups );
    int biggest_group = 0 ;
    std::vector<int> indices ;
    indices.reserve( data.nrows() );
    for( int i=0; i<ngroups; i++){
        const std::vector<int>& chunk = *chunks[orders[i]] ;
        push_back( indices, chunk ) ;
        biggest_group = std::max( biggest_group, (int)chunk.size() );
        group_sizes[i] = chunk.size() ;
    }
    
    DataFrameVisitors all_variables_visitors(data, data.names() ) ;
    data = all_variables_visitors.subset( indices, classes_grouped() ) ;
    
    // TODO: we own labels, so perhaps we can do an inplace sort, 
    //       to reuse its memory instead of creating a new data frame
    DataFrameVisitors labels_visitors( labels, vars) ;
    
    labels = labels_visitors.subset( orders, "data.frame" ) ;
    labels.attr( "vars" ) = R_NilValue ;
    
    data.attr( "group_sizes") = group_sizes ;
    data.attr( "biggest_group_size" ) = biggest_group ;
    data.attr( "labels" ) = labels ;
    return data ;
}

SEXP and_calls( List args ){
    int ncalls = args.size() ;
    if( !ncalls ) return Rf_ScalarLogical(TRUE) ;
    
    Rcpp::Armor<SEXP> res( args[0] ) ;
    SEXP and_symbol = Rf_install( "&" ) ;
    for( int i=1; i<ncalls; i++)
        res = Rcpp_lang3( and_symbol, res, args[i] ) ;
    return res ;
}

DataFrame filter_grouped( const GroupedDataFrame& gdf, List args, Environment env){
    // a, b, c ->  a & b & c
    Language call = and_calls( args ) ;
    
    const DataFrame& data = gdf.data() ;
    int nrows = data.nrows() ;
    LogicalVector test = no_init(nrows);
    
    LogicalVector g_test ;
    GroupedCallProxy call_proxy( call, gdf, env ) ;
    
    int ngroups = gdf.ngroups() ;
    GroupedDataFrame::group_iterator git = gdf.group_begin() ;
    for( int i=0; i<ngroups; i++, ++git){
        SlicingIndex indices = *git ;
        g_test  = call_proxy.get( indices );
        
        int chunk_size = indices.size() ;
        for( int j=0; j<chunk_size; j++){
            test[ indices[j] ] = g_test[j] ;  
        }
    }
    DataFrame res = subset( data, test, data.names(), classes_grouped() ) ;
    res.attr( "vars")   = data.attr("vars") ;
            
    return res ;
}

SEXP filter_not_grouped( DataFrame df, List args, Environment env){
    // a, b, c ->  a & b & c
    Language call = and_calls( args ) ;
    
    // replace the symbols that are in the data frame by vectors from the data frame
    // and evaluate the expression
    CallProxy proxy( call, df, env ) ;
    LogicalVector test = proxy.eval() ;
    
    DataFrame res = subset( df, test, df.names(), classes_not_grouped() ) ;
    return res ;
}

// [[Rcpp::export]]
SEXP filter_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return filter_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return filter_not_grouped( df, args, env) ;   
    }
}

template <typename Proxy>
SEXP structure_mutate( Proxy& call_proxy, const DataFrame& df, const CharacterVector& results_names, CharacterVector classes){
    int n = call_proxy.nsubsets() ;
    
    List out(n) ;
    CharacterVector names(n) ;
    
    CharacterVector input_names = df.names() ;
    int ncolumns = df.size() ;
    int i=0 ;
    for( ; i<ncolumns; i++){
        out[i] = call_proxy.get_variable(input_names[i]) ;
        SET_NAMED( out[i], 2 );
        names[i] = input_names[i] ;
    }
    for( int k=0; i<n; k++ ){
        String name = results_names[k] ;
        
        if( ! any( input_names.begin(), input_names.end(), name.get_sexp() ) ){
            SEXP x   = call_proxy.get_variable( name ) ; 
            out[i]   = x ;
            SET_NAMED( out[i], 2 );
            names[i] = name ;
            i++ ;
        }
    }
    
    
    out.attr("class") = classes ;
    set_rownames( out, df.nrows() ) ;
    out.names() = names;
    
    return out ;    
}

SEXP mutate_grouped(GroupedDataFrame gdf, List args, Environment env){
    const DataFrame& df = gdf.data() ;
    
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    GroupedCallProxy proxy(gdf, env) ;
    Shelter<SEXP> __ ;
    
    for( int i=0; i<nexpr; i++){
        proxy.set_call( args[i] );
        boost::scoped_ptr<Gatherer> gather( gatherer( proxy, gdf ) );
        proxy.input( results_names[i], __( gather->collect() ) ) ;
    }
    
    DataFrame res = structure_mutate( proxy, df, results_names, classes_grouped() ) ;
    res.attr( "vars")    = df.attr("vars") ;
    res.attr( "labels" ) = df.attr("labels" );
    res.attr( "index")   = df.attr("index") ;
    
    return res ;
}

SEXP mutate_not_grouped(DataFrame df, List args, Environment env){
    Shelter<SEXP> __ ;
    
    int nexpr = args.size() ;
    CharacterVector results_names = args.names() ;
    
    CallProxy call_proxy(df, env) ;
    for( int i=0; i<nexpr; i++){
        call_proxy.set_call( args[i] );
        
        // we need to protect the SEXP, that's what the Shelter does
        SEXP res = __( call_proxy.eval() ) ;
        call_proxy.input( results_names[i], res ) ;
        
    }
    
    DataFrame res = structure_mutate(call_proxy, df, results_names, classes_not_grouped() ) ;
    
    return res ;
}


// [[Rcpp::export]]
SEXP mutate_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return mutate_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return mutate_not_grouped( df, args, env) ;   
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
DataFrame arrange_impl( DataFrame data, List args, Environment env ){
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
	IntegerVector index = o.apply() ;
	
	DataFrameVisitors visitors( data, data.names() ) ;
	DataFrame res = visitors.subset(index, data.attr("class") ) ;
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

SEXP summarise_grouped(const GroupedDataFrame& gdf, List args, Environment env){
    DataFrame df = gdf.data() ;
    
    int nexpr = args.size() ;
    int nvars = gdf.nvars() ;
    CharacterVector results_names = args.names() ;
    List out(nexpr + nvars) ;
    CharacterVector names(nexpr + nvars) ;
    
    int i=0; 
    for( ; i<nvars; i++){
        out[i]      = gdf.label(i) ;
        SET_NAMED(out[i], 2) ;
        names[i]    = CHAR(PRINTNAME(gdf.symbol(i))) ;
    }
    
    LazyGroupedSubsets subsets(gdf) ;
    for( int k=0; k<nexpr; k++, i++ ){
        Result* res( get_handler( args[k], subsets ) ) ;
        
        // if we could not find a direct Result 
        // we can use a GroupedCalledReducer which will callback to R
        if( !res ) res = new GroupedCalledReducer( args[k], gdf, env) ;
        
        out[i] = res->process(gdf) ;
        names[i] = results_names[k] ;
        delete res;
    }
    
    return summarised_grouped_tbl_cpp(out, names, gdf );
}

SEXP summarise_not_grouped(DataFrame df, List args, Environment env){
    int nexpr = args.size() ;
    List out(nexpr) ;
    
    for( int i=0; i<nexpr; i++){
        LazySubsets subsets( df ) ;
        boost::scoped_ptr<Result> res( get_handler( args[i], subsets ) ) ;
        if(res) {
            out[i] = res->process( FullDataFrame(df) ) ;
        } else {
            out[i] = CallProxy( args[i], df, env).eval() ;
        }
    }
    
    return tbl_cpp( out, args.names(), 1 ) ;
}

// [[Rcpp::export]]
SEXP summarise_impl( DataFrame df, List args, Environment env){
    if( is<GroupedDataFrame>( df ) ){
        return summarise_grouped( GroupedDataFrame(df), args, env);    
    } else {
        return summarise_not_grouped( df, args, env) ;   
    }
}

// [[Rcpp::export]]
SEXP count_distinct(SEXP vec){ 
    SlicingIndex everything(0, Rf_length(vec) );
    boost::scoped_ptr<Result> res( count_distinct_result(vec) );
    if( !res ){
        std::stringstream ss ;
        #if RCPP_VERSION < Rcpp_Version(0,10,7)
        ss << "cannot handle object of type" << sexp_to_name( TYPEOF(vec) ) ;
        #else
        ss << "cannot handle object of type" << type2name(vec) ;
        #endif
        stop( ss.str() ) ;
    }
    return res->process(everything) ;
}

// [[Rcpp::export]]
List rbind_all( ListOf<DataFrame> dots ){
    int ndata = dots.size() ;
    int n = 0 ;
    for( int i=0; i<ndata; i++) n += dots[i].nrows() ;
    
    std::vector<Collecter*> columns ;
    std::vector<String> names ;
    int k=0 ;
    for( int i=0; i<ndata; i++){
        DataFrame df = dots[i] ;
        DataFrameVisitors visitors( df, df.names() ) ;
        int nrows = df.nrows() ;
        SlicingIndex index(k, nrows) ; 
        
        CharacterVector df_names = df.names() ;
        for( int j=0; j<df.size(); j++){
            SEXP source = df[j] ;
            String name = df_names[j] ;
            
            Collecter* coll = 0;
            int index = 0 ;
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
                Collecter* new_collecter = collecter(source, n ) ;
                
                // import data from this chunk 
                new_collecter->collect( SlicingIndex( k, nrows), source ) ;
                
                // import data from previous collecter
                new_collecter->collect( SlicingIndex(0, k), coll->get() ) ;
                
                // dispose the previous collecter and keep the new one. 
                delete coll ;
                columns[index] = new_collecter ;
                
            } else {
                stop( "incompatible type" ) ;
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
