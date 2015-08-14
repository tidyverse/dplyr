#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
List arrange_impl( DataFrame data, LazyDots dots ){
    if( data.size() == 0 ) return data ;
    check_valid_colnames(data) ;
    assert_all_white_list(data) ;

    // special case arrange() with no arguments for grouped data
    if( dots.size() == 0 && is<GroupedDataFrame>(data) ){
        DataFrame labels( data.attr( "labels" ) );
        OrderVisitors o(labels) ;
        IntegerVector index = o.apply() ;

        // reorganize
        labels = DataFrameSubsetVisitors( labels, labels.names() ).subset( index, labels.attr("class") );

        ListOf<IntegerVector> indices( data.attr("indices") ) ;
        int ngroups = indices.size() ;
        List new_indices(ngroups) ;
        IntegerVector master_index(data.nrows()) ;
        for( int i=0; i<ngroups; i++){
            new_indices[index[i]] = indices[i] ;
        }
        IntegerVector group_sizes = data.attr("group_sizes") ;
        IntegerVector new_group_sizes(ngroups);
        for( int i=0, k=0; i<ngroups; i++){
            IntegerVector idx = new_indices[i] ;
            IntegerVector new_group_index = seq(k, k + idx.size() - 1 );
            for( int j=0; j<idx.size(); j++, k++){
                master_index[k] = idx[j] ;
            }
            new_indices[i] = new_group_index ;
            new_group_sizes[i] = idx.size() ;
        }

        DataFrame res = DataFrameSubsetVisitors( data, data.names() ).subset( master_index, data.attr("class" ) ) ;
        res.attr( "labels" )  = labels ;
        res.attr( "indices" ) = new_indices ;
        res.attr( "vars"    ) = data.attr("vars" ) ;
        res.attr( "group_sizes" ) = new_group_sizes ;
        res.attr( "biggest_group_size" ) = data.attr("biggest_group_size") ;
        res.attr( "drop" ) = data.attr("drop") ;
        return res ;
    }

    if( dots.size() == 0 || data.nrows() == 0) return data ;

    int nargs = dots.size() ;
    if( is<GroupedDataFrame>(data) ){
        nargs += GroupedDataFrame(data).nvars() ;
    }

    List variables(nargs) ;
    LogicalVector ascending(nargs) ;

    int k = 0 ;
    if( is<GroupedDataFrame>(data) ){
        GroupedDataFrame gdf(data);
        for( ; k< gdf.nvars(); k++) {
            ascending[k] = true ;

            String s = PRINTNAME(gdf.symbol(k));
            variables[k] = data[s] ;
        }
    }

    for(int i=0; k<nargs; i++, k++){
        const Lazy& lazy = dots[i] ;

        Shield<SEXP> call_( lazy.expr() ) ;
        SEXP call = call_ ;
        bool is_desc = TYPEOF(call) == LANGSXP && Rf_install("desc") == CAR(call) ;

        CallProxy call_proxy(is_desc ? CADR(call) : call, data, lazy.env()) ;

        Shield<SEXP> v(call_proxy.eval()) ;
        if( !white_list(v) ){
            stop( "cannot arrange column of class '%s'", get_single_class(v) ) ;
        }

        if( Rf_inherits(v, "data.frame" ) ){
            DataFrame df(v) ;
            int nr = df.nrows() ;
            if( nr != data.nrows() ){
                stop( "data frame column with incompatible number of rows (%d), expecting : %d", nr, data.nrows() );
            }
        } else if( Rf_isMatrix(v) ) {
            SEXP dim = Rf_getAttrib(v, Rf_install( "dim" ) ) ;
            int nr = INTEGER(dim)[0] ;
            if( nr != data.nrows() ){
                stop( "matrix column with incompatible number of rows (%d), expecting : ", nr, data.nrows() ) ;
            }
        } else {
            if( Rf_length(v) != data.nrows() ){
                stop( "incorrect size (%d), expecting : %d", Rf_length(v), data.nrows() ) ;
            }
        }
        variables[k] = v ;
        ascending[k] = !is_desc ;
    }
    OrderVisitors o(variables, ascending, nargs) ;
    IntegerVector index = o.apply() ;

    DataFrameSubsetVisitors visitors( data, data.names() ) ;
    List res = visitors.subset(index, data.attr("class") ) ;

    if( is<GroupedDataFrame>(data) ){
        // so that all attributes are recalculated (indices ... )
        // see the lazyness feature in GroupedDataFrame
        // if we don't do that, we get the values of the un-arranged data
        // set for free from subset (#1064)
        res.attr("labels") = R_NilValue ;
        res.attr( "vars" )  = data.attr("vars" ) ;
        return GroupedDataFrame(res).data() ;
    }
    SET_ATTRIB(res, strip_group_attributes(res));
    return res ;
}
