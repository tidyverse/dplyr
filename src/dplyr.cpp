#define COMPILING_DPLYR
#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef Result* (*ResultPrototype)(SEXP, const DataFrame&) ;
typedef boost::unordered_map<SEXP,ResultPrototype> Result1_Map ;
  
#define MAKE_PROTOTYPE(__FUN__,__CLASS__)                               \
Result* __FUN__##_prototype( SEXP arg, const DataFrame& df ){           \
    const char* column_name = CHAR(PRINTNAME(arg)) ;                    \
    SEXP v = df[column_name] ;                                          \
    switch( TYPEOF(v) ){                                                \
        case INTSXP:  return new dplyr::__CLASS__<INTSXP,false>( v ) ;  \
        case REALSXP: return new dplyr::__CLASS__<REALSXP,false>( v ) ; \
        default: break ;                                                \
    }                                                                   \
    return 0 ;                                                          \
}
MAKE_PROTOTYPE(mean, Mean)
MAKE_PROTOTYPE(min, Min)
MAKE_PROTOTYPE(max, Max)
MAKE_PROTOTYPE(var, Var)
MAKE_PROTOTYPE(sd, Sd)
MAKE_PROTOTYPE(sum, Sum)

#define INSTALL_PROTOTYPE(__FUN__) prototypes[ Rf_install( #__FUN__ ) ] = __FUN__##_prototype ;

Result1_Map& get_1_arg_prototypes(){
    static Result1_Map prototypes ;
    if( !prototypes.size() ){ 
        INSTALL_PROTOTYPE(mean)
        INSTALL_PROTOTYPE(min)
        INSTALL_PROTOTYPE(max)
        INSTALL_PROTOTYPE(var)
        INSTALL_PROTOTYPE(sd)
        INSTALL_PROTOTYPE(sum)
    }
    return prototypes ;    
}

ResultPrototype get_1_arg(SEXP symbol){
    Result1_Map& prototypes = get_1_arg_prototypes() ;
    Result1_Map::iterator it = prototypes.find(symbol); 
    if( it == prototypes.end() ) return 0 ;
    return it->second ;
}

Result* get_result( SEXP call, const DataFrame& df){
    // no arguments
    int depth = Rf_length(call) ;
    if( depth == 1 && CAR(call) == Rf_install("n") )
        return new Count ;
    
    if( depth == 2 ){
        SEXP fun_symbol = CAR(call) ;
        SEXP arg1 = CADR(call) ;
        ResultPrototype reducer = get_1_arg( fun_symbol ) ;
        if( reducer ){
            Result* res = reducer( arg1, df ) ;
            return res ;    
        }
    }
    
    return 0 ;
}

// FIXME: this is too optimistic
bool can_simplify( SEXP call ){
    if( TYPEOF(call) == LANGSXP || TYPEOF(call) == LISTSXP ){
        int depth = Rf_length( call ) ;
        if( depth == 1 && CAR(call) == Rf_install("n") ) return true ;
        
        if( depth == 2 ){
            SEXP fun_symbol = CAR(call) ;
            ResultPrototype reducer = get_1_arg( fun_symbol ) ;
            if(reducer) return true ;        
        }
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
            
    return out.asSexp() ;
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

template <typename VisitorSet>
bool all_same_types(const VisitorSet& vx, const VisitorSet& vy){
    int n = vx.size() ;
    for( int i=0; i<n; i++){
        if( typeid(*vx.get(i)) != typeid(*vy.get(i)) )
            return false ;
    }
    return true ;
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

//' @export
// [[Rcpp::export]]
dplyr::BoolResult compatible_data_frame( DataFrame& x, DataFrame& y, bool ignore_col_order = false, bool convert = false ){
    int n = x.size() ;
    if( n != y.size() ) 
        return no_because( "not the same number of variables" ) ;
    
    CharacterVector names_x = clone<CharacterVector>(x.names()) ; 
    CharacterVector names_y = clone<CharacterVector>(y.names()) ; 
    if( ! ignore_col_order ){
        names_y.sort() ;
        names_x.sort() ;
    }
    
    for( int i=0; i<n; i++) 
        if( names_x[i] != names_y[i] )
            return no_because( "not the same variable names. ") ; 
    
    if( convert ){
        x = clone(x) ;
        y = clone(y) ;
        for( int i = 0; i<n; i++){
            x[i] = promote( x[i] ) ;
            y[i] = promote( y[i] ) ;
        }
    }
        
        
    DataFrameVisitors v_x( x, names_x );
    DataFrameVisitors v_y( y, names_y );
    if( ! all_same_types(v_x, v_y ) )
        return no_because( "different types" ) ;
    
    return yes() ;
}

//' @export
// [[Rcpp::export]]
dplyr::BoolResult equal_data_frame(DataFrame x, DataFrame y, bool ignore_col_order = false, bool ignore_row_order = false, bool convert = false ){
    BoolResult compat = compatible_data_frame(x, y, ignore_col_order, convert);
    if( !compat ) return compat ;
    
    int nrows = x.nrows() ;
    if( nrows != y.nrows() )
        return no_because( "different row sizes" );
    
    if( ignore_row_order ){
        DataFrameJoinVisitors visitors(x, y, x.names() ) ;
        for( int i=0; i<nrows; i++)
            if( !visitors.equal( i, -i-1) )
                return no_because( "different row" ) ;
    } else {
        typedef VisitorSetIndexMap<DataFrameJoinVisitors, int > Map ;
        DataFrameJoinVisitors visitors(x, y, x.names() ) ;
        Map map(visitors);  
        
        for( int i=0; i<nrows; i++) map[i]++ ;
        for( int i=0; i<nrows; i++){
            Map::iterator it = map.find(-i-1) ;
            if( it == map.end() || it->second < 0 ) 
                return no_because( "different subset" ) ;
            else
                it->second-- ;
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
    size_t biggest_group = 0 ;
    std::vector<int> indices ;
    indices.reserve( data.nrows() );
    for( int i=0; i<ngroups; i++){
        const std::vector<int>& chunk = *chunks[orders[i]] ;
        push_back( indices, chunk ) ;
        biggest_group = std::max( biggest_group, chunk.size() );
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
    
    DataFrame data = gdf.data() ;
    int nrows = data.nrows() ;
    LogicalVector test = no_init(nrows);
    
    LogicalVector g_test ;
    GroupedCallProxy call_proxy( call, gdf, env ) ;
    
    int ngroups = gdf.ngroups() ;
    // TODO: move this loop in GroupedCallProxy
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
    res.attr( "vars")   = gdf.attr("vars") ;
            
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
    DataFrame df = gdf.data() ;
    
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
    res.attr( "vars")    = gdf.attr("vars") ;
    res.attr( "labels" ) = gdf.attr("labels" );
    res.attr( "index")   = gdf.attr("index") ;
    
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
    for( int k=0; k<nexpr; k++, i++ ){
        Result* res( get_result( args[k], df ) ) ;
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
        boost::scoped_ptr<Result> res( get_result( args[i], df ) ) ;
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

