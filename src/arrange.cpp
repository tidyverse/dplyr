#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

// [[Rcpp::export]]
List arrange_impl( DataFrame data, LazyDots dots ){
    check_valid_colnames(data) ;
    assert_all_white_list(data) ;

    // special case arrange() with no arguments for grouped data
    if( dots.size() == 0 && is<GroupedDataFrame>(data) ){
        DataFrame labels( data.attr( "labels" ) );
        OrderVisitors o(labels) ;
        IntegerVector index = o.apply() ;

        // reorganize
        labels = DataFrameVisitors( labels, labels.names() ).subset( index, labels.attr("class") );

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

        DataFrame res = DataFrameVisitors( data, data.names() ).subset( master_index, data.attr("class" ) ) ;
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
        Shelter<SEXP> __ ;
        const Lazy& lazy = dots[i] ;

        SEXP call = lazy.expr() ;
        bool is_desc = TYPEOF(call) == LANGSXP && Rf_install("desc") == CAR(call) ;

        CallProxy call_proxy(is_desc ? CADR(call) : call, data, lazy.env()) ;

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
        variables[k] = v ;
        ascending[k] = !is_desc ;
    }
    OrderVisitors o(variables, ascending, nargs) ;
    IntegerVector index = o.apply() ;

    DataFrameVisitors visitors( data, data.names() ) ;
    List res = visitors.subset(index, data.attr("class") ) ;
    SET_ATTRIB(res, strip_group_attributes(res));
    return res ;
}

