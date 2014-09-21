#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

typedef dplyr_hash_map<SEXP,HybridHandler> HybridHandlerMap ;

template <template <int,bool> class Fun, bool narm>
Result* simple_prototype_impl( SEXP arg, bool is_summary ){
    switch( TYPEOF(arg) ){
        case INTSXP:  return new Fun<INTSXP,narm>( arg, is_summary ) ;
        case REALSXP: return new Fun<REALSXP,narm>( arg, is_summary ) ;
        default: break ;
    }
    return 0 ;
}

template <template <int,bool> class Fun>
Result* simple_prototype(  SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs == 0 ) return 0 ;
    SEXP arg = CADR(call) ;
    bool is_summary = false ;
    if( TYPEOF(arg) == SYMSXP ){
      if( subsets.count(arg) ) {
          is_summary = subsets.is_summary(arg) ;
          arg = subsets.get_variable(arg) ;
      }
      else return 0 ;
    }

    if( nargs == 1 ){
        return simple_prototype_impl<Fun, false>( arg, is_summary ) ;
    } else if(nargs == 2 ){
        SEXP arg2 = CDDR(call) ;
        // we know how to handle fun( ., na.rm = TRUE/FALSE )
        if( TAG(arg2) == R_NaRmSymbol ){
            SEXP narm = CAR(arg2) ;
            if( TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1 ){
                if( LOGICAL(narm)[0] == TRUE ){
                    return simple_prototype_impl<Fun, true>( arg, is_summary ) ;
                } else {
                    return simple_prototype_impl<Fun, false>( arg, is_summary ) ;
                }
            }
        }
    }
    return 0 ;
}

template< template <int, bool> class Tmpl, bool narm>
Result* minmax_prototype_impl(SEXP arg, bool is_summary){
    switch( TYPEOF(arg) ){
        case INTSXP:
            if( Rf_inherits(arg, "Date" ) || Rf_inherits(arg, "POSIXct" ) )
                return typed_processor( Tmpl<INTSXP, narm>(arg, is_summary), arg  ) ;
            return new Tmpl<INTSXP,narm>( arg, is_summary ) ;
        case REALSXP:
            if( Rf_inherits(arg, "Date" ) || Rf_inherits(arg, "POSIXct" ) )
                return typed_processor( Tmpl<INTSXP, narm>(arg, is_summary), arg  ) ;
            return new Tmpl<REALSXP,narm>( arg, is_summary ) ;
        default: break ;
    }
    return 0 ;
}

template< template <int, bool> class Tmpl>
Result* minmax_prototype( SEXP call, const LazySubsets& subsets, int nargs ){
    using namespace dplyr ;

    if( nargs == 1 ) return 0 ;

    // the first argument is the data to operate on
    SEXP arg = CADR(call) ;
    bool is_summary = false ;
    if( TYPEOF(arg) == SYMSXP ){
      if( subsets.count(arg) ) {
          is_summary = subsets.is_summary(arg) ;
          arg = subsets.get_variable(arg) ;
      }
      else return 0 ;
    }

    if( nargs == 1 ){
        return minmax_prototype_impl<Tmpl,false>(arg, is_summary) ;
    } else if( nargs == 2 ){
        SEXP arg2 = CDDR(call) ;
        // we know how to handle fun( ., na.rm = TRUE/FALSE )
        if( TAG(arg2) == R_NaRmSymbol ){
            SEXP narm = CAR(arg2) ;
            if( TYPEOF(narm) == LGLSXP && LENGTH(narm) == 1 ){
                if( LOGICAL(narm)[0] == TRUE ){
                    return minmax_prototype_impl<Tmpl,true>(arg, is_summary) ;
                } else {
                    return minmax_prototype_impl<Tmpl,false>(arg, is_summary) ;
                }
            }
        }
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
    if( TYPEOF(arg) != SYMSXP || !subsets.count(arg) || Rf_length(call) != 2) {
        stop( "Input to n_distinct() must be a single variable name from the data set" ) ;
    }
    return count_distinct_result(subsets.get_variable(arg)) ;
}

Result* row_number_prototype(SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs >  1 ) return 0;

    if( nargs == 0 ) return new RowNumber_0() ;

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

Result* ntile_prototype( SEXP call, const LazySubsets& subsets, int nargs ){
    if( nargs != 2 ) return 0;

    // handle 2nd arg
    SEXP ntiles = CADDR(call) ;
    double number_tiles ;
    try{
        number_tiles = as<int>(ntiles) ;
    } catch( ... ){
        stop("could not convert n to scalar integer") ;
    }

    Armor<SEXP> data( CADR(call) );
    if( TYPEOF(data) == LANGSXP && CAR(data) == Rf_install("desc") ){
        data = CADR(data) ;

        if( TYPEOF(data) == SYMSXP ){
          if( subsets.count(data) ) data = subsets.get_variable(data) ;
          else return 0 ;
        }
        switch( TYPEOF(data) ){
            case INTSXP:  return new Ntile<INTSXP,  false>( data, number_tiles ) ;
            case REALSXP: return new Ntile<REALSXP, false>( data, number_tiles ) ;
            case STRSXP:  return new Ntile<STRSXP,  false>( data, number_tiles ) ;
            default: break;
        }
    }
    if( TYPEOF(data) == SYMSXP ){
      if( subsets.count(data) ) data = subsets.get_variable(data) ;
      else return 0 ;
    }
    switch( TYPEOF(data) ){
        case INTSXP:  return new Ntile<INTSXP ,true>( data, number_tiles ) ;
        case REALSXP: return new Ntile<REALSXP,true>( data, number_tiles ) ;
        case STRSXP:  return new Ntile<STRSXP ,true>( data, number_tiles ) ;
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

bool argmatch( const std::string& target, const std::string& s){
    if( s.size() > target.size() ) return false ;
    return target.compare( 0, s.size(), s ) == 0 ;
}

template <int RTYPE, template <int> class Without >
Result* first_noorder_default( Vector<RTYPE> data, Vector<RTYPE> def ){
     return new Without<RTYPE>(data, def[0] );
}

template <int RTYPE, template <int, int> class With>
Result* first_with( Vector<RTYPE> data, SEXP order ){
    switch( TYPEOF(order) ){
    case INTSXP: return new With<RTYPE, INTSXP>( data, order );
    case REALSXP: return new With<RTYPE, REALSXP>( data, order );
    case STRSXP: return new With<RTYPE, STRSXP>( data, order );
    default: break ;
    }
    return 0 ;
}

template <int RTYPE, template <int, int> class With>
Result* first_with_default( Vector<RTYPE> data, SEXP order, Vector<RTYPE> def ){
    switch( TYPEOF(order) ){
    case INTSXP: return new With<RTYPE, INTSXP>( data, order, def[0] );
    case REALSXP: return new With<RTYPE, REALSXP>( data, order, def[0] );
    case STRSXP: return new With<RTYPE, STRSXP>( data, order, def[0] );
    default: break ;
    }
    return 0 ;
}


template < template <int> class Without, template <int, int> class With >
Result* first_prototype( SEXP call, const LazySubsets& subsets, int nargs){
    // has to have one argument
    if( nargs == 0 ) return 0 ;

    SEXP tag = TAG(CDR(call)) ;
    if( tag != R_NilValue && tag != Rf_install("x") ){
        stop( "the first argument of 'first' should be either 'x' or unnamed" ) ;
    }
    SEXP data = CADR(call) ;
    if( TYPEOF(data) == SYMSXP ) data = subsets.get_variable(data) ;
    
    // easy case : just a single variable: first(x)
    if( nargs == 1 ){
        switch( TYPEOF(data) ){
        case INTSXP: return new Without<INTSXP>(data) ;
        case REALSXP: return new Without<REALSXP>(data) ;
        case STRSXP: return new Without<STRSXP>(data) ;
        default: break ;
        }
    } else {
        SEXP order_by = R_NilValue ;
        SEXP def      = R_NilValue ;

        SEXP p = CDDR(call) ;
        while( p != R_NilValue ){
            SEXP tag = TAG(p) ;
            if( tag == R_NilValue ) stop( "all arguments of 'first' after the first one should be named" ) ;
            std::string argname = CHAR(PRINTNAME(tag));
            if( argmatch( "order_by", argname ) ){
                order_by = CAR(p) ;
            } else if( argmatch( "default", argname ) ){
                def = CAR(p) ;
            } else {
                stop("argument to 'first' does not match either 'default' or 'order_by' ") ;
            }

            p = CDR(p) ;
        }

        // handle cases
        if( def == R_NilValue ){

            // then we know order_by is not NULL, we only handle the case where
            // order_by is a symbol and that symbol is in the data
            if( TYPEOF(order_by) != SYMSXP || ! subsets.count(order_by) ){
                stop("invalid order_by") ;
            }
            order_by = subsets.get_variable(order_by) ;

            switch( TYPEOF(data) ){
                case INTSXP: return first_with<INTSXP, With>( data, order_by ) ;
                case REALSXP: return first_with<REALSXP, With>( data, order_by ) ;
                case STRSXP: return first_with<STRSXP, With>( data, order_by ) ;
                default: break ;
            }

        } else {
            if( order_by == R_NilValue ){
                switch( TYPEOF(data) ){
                    case INTSXP: return first_noorder_default<INTSXP, Without>(data, def) ;
                    case REALSXP: return first_noorder_default<REALSXP, Without>(data, def) ;
                    case STRSXP: return first_noorder_default<STRSXP, Without>(data, def) ;
                    default: break ;
                }
            } else {
                if( TYPEOF(order_by) != SYMSXP || ! subsets.count(order_by) ){
                    stop("invalid order_by") ;
                }
                order_by = subsets.get_variable(order_by) ;

                switch( TYPEOF(data) ){
                    case INTSXP: return first_with_default<INTSXP, With>(data, order_by, def) ;
                    case REALSXP: return first_with_default<REALSXP,With>(data, order_by, def) ;
                    case STRSXP: return first_with_default<STRSXP,With>(data, order_by, def) ;
                    default: break ;
                }
            }
        }


    }
    return 0;
}

template <int RTYPE>
Result* nth_noorder_default( Vector<RTYPE> data, int idx, Vector<RTYPE> def ){
     return new Nth<RTYPE>(data, idx, def[0] );
}

template <int RTYPE>
Result* nth_noorder_default__typed( Vector<RTYPE> data, int idx, Vector<RTYPE> def ){
     return typed_processor( Nth<RTYPE>(data, idx, def[0] ), data );
}


template <int RTYPE>
Result* nth_with( Vector<RTYPE> data, int idx, SEXP order ){
    switch( TYPEOF(order) ){
    case LGLSXP: return new NthWith<RTYPE, LGLSXP>( data, idx, order );
    case INTSXP: return new NthWith<RTYPE, INTSXP>( data, idx, order );
    case REALSXP: return new NthWith<RTYPE, REALSXP>( data, idx, order );
    case STRSXP: return new NthWith<RTYPE, STRSXP>( data, idx, order );
    default: break ;
    }
    return 0 ;
}

template <int RTYPE>
Result* nth_with__typed( Vector<RTYPE> data, int idx, SEXP order ){
    switch( TYPEOF(order) ){
    case LGLSXP:  return typed_processor( NthWith<RTYPE, LGLSXP>( data, idx, order ), data ) ;
    case INTSXP:  return typed_processor( NthWith<RTYPE, INTSXP>( data, idx, order ), data ) ;
    case REALSXP: return typed_processor( NthWith<RTYPE, REALSXP>( data, idx, order), data ) ;
    case STRSXP:  return typed_processor( NthWith<RTYPE, STRSXP>( data, idx, order ), data ) ;
    default: break ;
    }
    return 0 ;
}


template <int RTYPE>
Result* nth_with_default( Vector<RTYPE> data, int idx, SEXP order, Vector<RTYPE> def ){
    switch( TYPEOF(order) ){
    case LGLSXP: return new NthWith<RTYPE, LGLSXP>( data, idx, order, def[0] );
    case INTSXP: return new NthWith<RTYPE, INTSXP>( data, idx, order, def[0] );
    case REALSXP: return new NthWith<RTYPE, REALSXP>( data, idx, order, def[0] );
    case STRSXP: return new NthWith<RTYPE, STRSXP>( data, idx, order, def[0] );
    default: break ;
    }
    return 0 ;
}

template <int RTYPE>
Result* nth_with_default__typed( Vector<RTYPE> data, int idx, SEXP order, Vector<RTYPE> def ){
    switch( TYPEOF(order) ){
    case LGLSXP:  return typed_processor( NthWith<RTYPE, LGLSXP>( data, idx, order, def[0] ), data );
    case INTSXP:  return typed_processor( NthWith<RTYPE, INTSXP>( data, idx, order, def[0] ), data );
    case REALSXP: return typed_processor( NthWith<RTYPE, REALSXP>( data, idx, order, def[0]), data );
    case STRSXP:  return typed_processor( NthWith<RTYPE, STRSXP>( data, idx, order, def[0] ), data );
    default: break ;
    }
    return 0 ;
}


Result* nth_prototype( SEXP call, const LazySubsets& subsets, int nargs){
    // has to have at least two arguments
    if( nargs < 2 ) return 0 ;

    SEXP tag = TAG(CDR(call)) ;
    if( tag != R_NilValue && tag != Rf_install("x") ){
        stop( "the first argument of 'nth' should be either 'x' or unnamed" ) ;
    }
    SEXP data = CADR(call) ;
    if( TYPEOF(data) == SYMSXP ) {
        if( ! subsets.count(data) ){
            std::stringstream s ;
            s << "could not find variable '" << CHAR(PRINTNAME(data)) << "'" ;
            stop(s.str()) ;
        }
        data = subsets.get_variable(data) ;
    }

    tag = TAG(CDDR(call)) ;
    if( tag != R_NilValue && tag != Rf_install("n") ){
        stop( "the second argument of 'first' should be either 'n' or unnamed" ) ;
    }
    SEXP nidx = CADDR(call) ;
    if( ( TYPEOF(nidx) != REALSXP && TYPEOF(nidx) != INTSXP ) || LENGTH(nidx) != 1 ){
        stop("'n' should be a scalar integer") ;
    }
    int idx = as<int>(nidx) ;

    // easy case : just a single variable: first(x,n)
    if( nargs == 2 ){
        switch( TYPEOF(data) ){
        case INTSXP: 
            {
                if( Rf_inherits(data, "Date") || Rf_inherits(data, "POSIXct") ) 
                    return typed_processor( Nth<INTSXP>(data, idx), data ) ;
                return new Nth<INTSXP>(data, idx) ;
            }
        case REALSXP: 
            {
                if( Rf_inherits(data, "Date") || Rf_inherits(data, "POSIXct") ) 
                    return typed_processor( Nth<REALSXP>(data, idx), data ) ;
                return new Nth<REALSXP>(data, idx) ;
            }
        case STRSXP: return new Nth<STRSXP>(data, idx) ;
        case LGLSXP: return new Nth<LGLSXP>(data, idx) ;
        
        default: break ;
        }
    } else {
        // now get `order_by` and default

        SEXP order_by = R_NilValue ;
        SEXP def      = R_NilValue ;

        SEXP p = CDR(CDDR(call)) ;
        while( p != R_NilValue ){
            SEXP tag = TAG(p) ;
            if( tag == R_NilValue ) stop( "all arguments of 'first' after the first one should be named" ) ;
            std::string argname = CHAR(PRINTNAME(tag));
            if( argmatch( "order_by", argname ) ){
                order_by = CAR(p) ;
            } else if( argmatch( "default", argname ) ){
                def = CAR(p) ;
            } else {
                stop("argument to 'first' does not match either 'default' or 'order_by' ") ;
            }

            p = CDR(p) ;
        }


        // handle cases
        if( def == R_NilValue ){

            // then we know order_by is not NULL, we only handle the case where
            // order_by is a symbol and that symbol is in the data
            if( TYPEOF(order_by) != SYMSXP || ! subsets.count(order_by) ){
                stop("invalid order_by") ;
            }
            order_by = subsets.get_variable(order_by) ;

            switch( TYPEOF(data) ){
                case LGLSXP: return nth_with<LGLSXP>( data, idx, order_by ) ;
                case INTSXP:
                    {
                        if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                            return nth_with__typed<INTSXP>( data, idx, order_by ) ;
                        return nth_with<INTSXP>( data, idx, order_by ) ;
                    }
                case REALSXP: 
                    {
                        if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                            return nth_with__typed<REALSXP>( data, idx, order_by ) ;
                        return nth_with<REALSXP>( data, idx, order_by ) ;
                    }
                case STRSXP: return nth_with<STRSXP>( data, idx, order_by ) ;
                default: break ;
            }

        } else {
            if( order_by == R_NilValue ){
                switch( TYPEOF(data) ){
                    case LGLSXP: return nth_noorder_default<LGLSXP>(data, idx, def) ;
                    case INTSXP: 
                        {
                            if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                                return nth_noorder_default__typed<INTSXP>( data, idx, order_by ) ;
                            return nth_noorder_default<INTSXP>(data, idx, def) ;
                        }
                    case REALSXP: 
                        {
                            if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                                return nth_noorder_default__typed<REALSXP>( data, idx, order_by ) ;
                            return nth_noorder_default<REALSXP>(data, idx, def) ;
                        }
                    case STRSXP: return nth_noorder_default<STRSXP>(data, idx, def) ;
                    default: break ;
                }
            } else {
                if( TYPEOF(order_by) != SYMSXP || ! subsets.count(order_by) ){
                    stop("invalid order_by") ;
                }
                order_by = subsets.get_variable(order_by) ;

                switch( TYPEOF(data) ){
                    case LGLSXP: return nth_with_default<LGLSXP>(data, idx, order_by, def) ;
                    case INTSXP: {
                            if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                                return nth_with_default__typed<INTSXP>( data, idx, order_by, def ) ;
                            return nth_with_default<INTSXP>(data, idx, order_by, def) ;
                    }
                    case REALSXP: {
                            if( Rf_inherits( data, "Date" ) || Rf_inherits( data, "POSIXct" ) )
                                return nth_with_default__typed<REALSXP>( data, idx, order_by, def ) ;
                            return nth_with_default<REALSXP>(data, idx, order_by, def) ;
                    }
                    case STRSXP: return nth_with_default<STRSXP>(data, idx, order_by, def) ;
                    default: break ;
                }
            }
        }

    }
    return 0;
}


HybridHandlerMap& get_handlers(){
    static HybridHandlerMap handlers ;
    if( !handlers.size() ){
        handlers[ Rf_install( "n")               ] = count_prototype ;
        handlers[ Rf_install( "n_distinct" )     ] = count_distinct_prototype ;
        handlers[ Rf_install( "row_number" )     ] = row_number_prototype ;
        handlers[ Rf_install( "ntile" )          ] = ntile_prototype ;

        handlers[ Rf_install( "min" )            ] = minmax_prototype<dplyr::Min> ;
        handlers[ Rf_install( "max" )            ] = minmax_prototype<dplyr::Max> ;

        handlers[ Rf_install( "mean" )           ] = simple_prototype<dplyr::Mean> ;
        handlers[ Rf_install( "var" )            ] = simple_prototype<dplyr::Var> ;
        handlers[ Rf_install( "sd")              ] = simple_prototype<dplyr::Sd> ;
        handlers[ Rf_install( "sum" )            ] = simple_prototype<dplyr::Sum>;

        handlers[ Rf_install( "min_rank" )       ] = rank_impl_prototype<dplyr::internal::min_rank_increment> ;
        handlers[ Rf_install( "percent_rank" )   ] = rank_impl_prototype<dplyr::internal::percent_rank_increment> ;
        handlers[ Rf_install( "dense_rank" )     ] = rank_impl_prototype<dplyr::internal::dense_rank_increment> ;
        handlers[ Rf_install( "cume_dist" )      ] = rank_impl_prototype<dplyr::internal::cume_dist_increment> ;

        // handlers[ Rf_install( "cumsum")          ] = cumfun_prototype<CumSum> ;
        // handlers[ Rf_install( "cummin")          ] = cumfun_prototype<CumMin> ;
        // handlers[ Rf_install( "cummax")          ] = cumfun_prototype<CumMax> ;

        // handlers[ Rf_install( "lead" )           ] = lead_prototype ;
        // handlers[ Rf_install( "lag" )            ] = lag_prototype ;

        handlers[ Rf_install( "first" ) ] = first_prototype<dplyr::First, dplyr::FirstWith> ;
        handlers[ Rf_install( "last" ) ]  = first_prototype<dplyr::Last, dplyr::LastWith> ;
        handlers[ Rf_install( "nth" ) ]  = nth_prototype ;
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
DataFrame subset( DataFrame x, DataFrame y, const Index& indices_x, const Index& indices_y, CharacterVector by_x, CharacterVector by_y , CharacterVector classes ){
    // first the joined columns 
    DataFrameJoinVisitors join_visitors(x, y, by_x, by_y) ;
    int n_join_visitors = join_visitors.size() ;
    
    // then columns from x but not y
    CharacterVector all_x_columns = x.names() ;
    CharacterVector x_columns( all_x_columns.size() - n_join_visitors ) ;
    for( int i=0, k=0; i<all_x_columns.size(); i++){
        SEXP name = all_x_columns[i] ;
        if( std::find(by_x.begin(), by_x.end(), name) == by_x.end() ) {
            x_columns[k++] = name ;
        }
    }
    DataFrameVisitors visitors_x(x, x_columns) ;
    int nv_x = visitors_x.size() ;
    
    // then columns from y but not x
    CharacterVector all_y_columns = y.names() ;
    CharacterVector y_columns( all_y_columns.size() - n_join_visitors ) ;
    for( int i=0, k=0; i<all_y_columns.size(); i++){
        SEXP name = all_y_columns[i] ;
        if( std::find(by_y.begin(), by_y.end(), name) == by_y.end() ) {
            y_columns[k++] = name ;
        }
    }
    DataFrameVisitors visitors_y(y, y_columns) ;
    int nv_y = visitors_y.size() ;
    
    // construct out object
    int nrows = indices_x.size() ;
    List out(n_join_visitors+nv_x+nv_y);
    CharacterVector names(n_join_visitors+nv_x+nv_y) ;
    int k=0;
    
    // ---- join visitors
    for( ; k<n_join_visitors; k++){
        out[k] = join_visitors.get(k)->subset(indices_x) ;
        names[k] = by_x[k] ;
    }
    
    for( int i=0; i<nv_x; k++, i++){
        out[k] = visitors_x.get(i)->subset(indices_x) ;
        String col_name = x_columns[i] ;

        if( std::find( by_x.begin(), by_x.end(), col_name.get_sexp() ) != by_x.end() ){
            // if the variable is from by_x, just use it verbatim
        } else if( std::find(y_columns.begin(), y_columns.end(), col_name.get_sexp()) != y_columns.end() ) {
            // if it is not, but is also in y, then suffix with .x
            col_name += ".x" ;
        } else {
            // otherwise just use verbatim
        }

        names[k] = col_name ;
    }
    for( int i=0; i<nv_y; i++, k++){
        String col_name = y_columns[i] ;

        // we suffix by .y if this column is in x_columns

        if( std::find(x_columns.begin(), x_columns.end(), col_name.get_sexp()) != x_columns.end() ){
            col_name += ".y" ;
        }

        out[k] = visitors_y.get(i)->subset(indices_y) ;
        names[k] = col_name ;
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
template <typename TargetContainer, typename SourceContainer>
void push_back_right( TargetContainer& x, const SourceContainer& y ){
    // x.insert( x.end(), y.begin(), y.end() ) ;
    int n = y.size() ;
    for( int i=0; i<n; i++){
        x.push_back( -y[i]-1 ) ;
    }
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
DataFrame semi_join_impl( DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y ){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by_x, by_y) ;
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
DataFrame anti_join_impl( DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by_x, by_y) ;
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
DataFrame inner_join_impl( DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by_x, by_y) ;
    Map map(visitors);

    int n_x = x.nrows(), n_y = y.nrows() ;

    std::vector<int> indices_x ;
    std::vector<int> indices_y ;

    if( n_x <= n_y ){
        // train the map in terms of x
        train_push_back( map, n_x ) ;

        for( int i=0; i<n_y; i++){
            // find indices for rows in x that match the row i in y
            Map::iterator it = map.find(-i-1) ;
            if( it != map.end() ){
                push_back( indices_x, it->second );
                push_back( indices_y, i, it->second.size() ) ;
            }
        }
    } else {
        train_push_back_right( map, n_y ) ;

        for( int i=0; i<n_x; i++){
            Map::iterator it = map.find(i) ;
            if( it != map.end() ){
                push_back_right( indices_y, it->second );
                push_back( indices_x, i, it->second.size() ) ;
            }
        }
    }
    return subset( x, y, indices_x, indices_y, by_x, by_y, x.attr( "class") );
}

// [[Rcpp::export]]
DataFrame left_join_impl( DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y ){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(y, x, by_x, by_y) ;
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
    return subset( x, y, indices_x, indices_y, by_x, by_y, x.attr( "class" ) ) ;
}

// [[Rcpp::export]]
DataFrame right_join_impl( DataFrame x, DataFrame y, CharacterVector by_x, CharacterVector by_y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    DataFrameJoinVisitors visitors(x, y, by_x, by_y) ;
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
    return subset( x, y, indices_x, indices_y, by_x, by_y, x.attr( "class" ) ) ;
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
    DataFrameJoinVisitors visitors(x, y, x.names(), x.names() ) ;
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
    DataFrameJoinVisitors visitors(x, y, x.names(), x.names() ) ;
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
    DataFrameJoinVisitors visitors(x, y, x.names(), x.names() ) ;
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
    DataFrameJoinVisitors visitors(y, x, y.names(), y.names() ) ;
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
    DataFrameJoinVisitors visitors(y, x, x.names(), x.names() ) ;
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
    if( !symbols.size() )
        stop("no variables to group by") ;
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

    train_push_back( map, data.nrows() ) ;

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
    if( TYPEOF(tmp) != INTSXP &&  TYPEOF(tmp) != REALSXP ){
        stop( "slice condition does not evaluate to an integer or numeric vector. " ) ;
    }
    return tmp ;
}

class CountIndices {
public:
    CountIndices( int nr_, IntegerVector test_ ) : nr(nr_), test(test_), n_pos(0), n_neg(0){
    
        for( int j=0; j<test.size(); j++){ 
            int i = test[j] ;
            if( i > 0 && i <= nr ) {
                n_pos++ ;
            } else if( i < 0 && i >= -nr ){
                n_neg++ ;    
            }
        }
        
        if( n_neg > 0 && n_pos > 0 ){
            std::stringstream s;
            s << "found " << n_pos << " positive indices and " << n_neg << " negative indices" ;  
            stop(s.str()) ;    
        }
        
    }
    
    inline bool is_positive() const { return n_pos > 0 ; }
    inline int get_n_positive() const { return n_pos; }
    inline int get_n_negative() const { return n_neg; }
    
private:
    int nr ;
    IntegerVector test ;
    int n_pos ;
    int n_neg ;   
} ;
   
SEXP slice_grouped(GroupedDataFrame gdf, const List& args, const DataDots& dots){
    typedef GroupedCallProxy<GroupedDataFrame, LazyGroupedSubsets> Proxy ;

    const DataFrame& data = gdf.data() ;
    Environment env = dots.envir(0);
    CharacterVector names = data.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }

    // we already checked that we have only one expression
    Call call( (SEXP)args[dots.expr_index(0)] ) ;

    std::vector<int> indx ; indx.reserve(1000) ;

    IntegerVector g_test ;
    Proxy call_proxy( call, gdf, env ) ;

    int ngroups = gdf.ngroups() ;
    GroupedDataFrame::group_iterator git = gdf.group_begin() ;
    for( int i=0; i<ngroups; i++, ++git){
        SlicingIndex indices = *git ;
        int nr = indices.size() ;
        g_test = check_filter_integer_result( call_proxy.get( indices ) ) ;
        CountIndices counter( indices.size(), g_test ) ;
        
        if( counter.is_positive() ){
            // positive indexing
            int ntest = g_test.size() ;
            for( int j=0; j<ntest; j++){
                if( g_test[j] <= nr ){
                    indx.push_back( indices[g_test[j]-1] ) ;
                }
            }        
        } else {
            // negative indexing
            std::set<int> drop ;
            int n = g_test.size() ;
            for( int j=0; j<n; j++){
                drop.insert( -g_test[j] ) ;
            }
            int n_drop = drop.size() ;
            std::set<int>::const_iterator drop_it = drop.begin() ;
            
            int k = 0, j = 0 ;
            while( drop_it != drop.end() ){
                int next_drop = *drop_it - 1;
                while( j < next_drop ){
                    indx.push_back( indices[j++] ) ;
                    k++ ;
                }
                j++ ;
                ++drop_it ;
            }
            while( k < nr - n_drop){
                indx.push_back( indices[j++] ) ;
                k++ ;
            }
            
        }
    }

    DataFrame res = subset( data, indx, names, classes_grouped<GroupedDataFrame>() ) ;
    res.attr( "vars")   = data.attr("vars") ;

    return res ;

}

SEXP slice_not_grouped( const DataFrame& df, const List& args, const DataDots& dots){
    CharacterVector names = df.names() ;
    SymbolSet set ;
    for( int i=0; i<names.size(); i++){
        set.insert( Rf_install( names[i] ) ) ;
    }

    Environment env = dots.envir(0) ;
    Call call( (SEXP)args[dots.expr_index(0)] );
    CallProxy proxy( call, df, env ) ;
    int nr = df.nrows() ;
    
    IntegerVector test = check_filter_integer_result(proxy.eval()) ;

    int n = test.size() ;
    
    // count the positive and negatives
    CountIndices counter(nr, test) ;
    
    // just positives -> one based subset
    if( counter.is_positive() ){
        int n_pos = counter.get_n_positive() ;
        std::vector<int> idx(n_pos) ;
        int j=0 ;
        for( int i=0; i<n_pos; i++){
            while( test[j] > nr ) j++ ;
            idx[i] = test[j] - 1 ;   
        }
        
        return subset( df, idx, df.names(), classes_not_grouped() ) ;        
    }
    
    // just negatives (out of range is dealt with early in CountIndices). 
    std::set<int> drop ;
    for( int i=0; i<n; i++){
        drop.insert( -test[i] ) ;
    }
    int n_drop = drop.size() ;
    std::vector<int> indices(nr - n_drop) ; 
    std::set<int>::const_iterator drop_it = drop.begin() ;
    
    int i = 0, j = 0 ;
    while( drop_it != drop.end() ){
        int next_drop = *drop_it - 1;
        while( j < next_drop ){
            indices[i++] = j++ ; 
        }
        j++ ;
        ++drop_it ;
    }
    while( i < nr - n_drop){
        indices[i++] = j++ ; 
    }
    
    DataFrame res = subset( df, indices, df.names(), classes_not_grouped() ) ;
    return res ;

}

// [[Rcpp::export]]
SEXP slice_impl( DataFrame df, List args, Environment env){
    if( args.size() == 0 ) return df ;
    if( args.size() != 1 )
        stop( "slice only accepts one expression" );
    DataDots dots(env) ;
    if( is<GroupedDataFrame>(df) ){
        return slice_grouped( GroupedDataFrame(df), args, dots ) ;
    } else {
        return slice_not_grouped(df, args, dots ) ;
    }
}

template <typename Data>
SEXP structure_mutate( const NamedListAccumulator<Data>& accumulator, const DataFrame& df, CharacterVector classes){
    List res = accumulator ;
    res.attr("class") = classes ;
    set_rownames( res, df.nrows() ) ;
    res.attr( "vars")     = df.attr("vars") ;
    res.attr( "labels" )  = df.attr("labels" );
    res.attr( "index")    = df.attr("index") ;
    res.attr( "indices" ) = df.attr("indices" ) ;

    return res ;
}

void check_not_groups(const CharacterVector& result_names, const RowwiseDataFrame& gdf){}
void check_not_groups(const CharacterVector& result_names, const GroupedDataFrame& gdf){
    int n = result_names.size() ;
    for( int i=0; i<n; i++){
        if( gdf.has_group( result_names[i] ) )
            stop( "cannot modify grouping variable" ) ;
    }
}

template <typename Data, typename Subsets>
SEXP mutate_grouped(const DataFrame& df, List args, const DataDots& dots){
    typedef GroupedCallProxy<Data, Subsets> Proxy;

    Data gdf(df);
    int nexpr = dots.size() ;
    CharacterVector results_names = args.names() ;
    check_not_groups(results_names, gdf);

    Environment env = dots.envir(0) ;
    Proxy proxy(gdf, env) ;
    Shelter<SEXP> __ ;

    NamedListAccumulator<Data> accumulator ;
    int ncolumns = df.size() ;
    CharacterVector column_names = df.names() ;
    for( int i=0; i<ncolumns; i++){
        accumulator.set( column_names[i], df[i] ) ;
    }

    for( int i=0; i<nexpr; i++){
        Rcpp::checkUserInterrupt() ;

        env = dots.envir(i) ;
        proxy.set_env( env ) ;
        SEXP call = args[dots.expr_index(i)] ;
        SEXP name = results_names[dots.expr_index(i)] ;
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
                    Replicator* rep = constant_replicator<Data>(v, gdf.nrows() );
                    variable = __( rep->collect() );
                    delete rep ;
                } else {
                    Replicator* rep = replicator<Data>(v, gdf) ;
                    variable = __( rep->collect() );
                    delete rep ;
                }
            }

        } else if(TYPEOF(call) == LANGSXP){
            proxy.set_call( call );
            Gatherer* gather = gatherer<Data, Subsets>( proxy, gdf, name ) ;
            variable = __( gather->collect() ) ;
            delete gather ;
        } else if(Rf_length(call) == 1) {
            boost::scoped_ptr<Gatherer> gather( constant_gatherer<Data, Subsets>( call, gdf.nrows() ) );
            variable = __( gather->collect() ) ;
        } else if( Rf_isNull(call) ){
            accumulator.rm(name) ;
            continue ;
        } else {
            stop( "cannot handle" ) ;
        }

        proxy.input( name, variable ) ;
        accumulator.set( name, variable) ;
    }

    return structure_mutate(accumulator, df, classes_grouped<Data>() );
}

SEXP mutate_not_grouped(DataFrame df, List args, const DataDots& dots){
    Shelter<SEXP> __ ;
    Environment env = dots.envir(0) ;

    int nexpr = dots.size() ;
    CharacterVector results_names = args.names() ;

    NamedListAccumulator<DataFrame> accumulator ;
    int nvars = df.size() ;
    CharacterVector df_names = df.names() ;
    for( int i=0; i<nvars; i++){
        accumulator.set( df_names[i], df[i] ) ;
    }

    CallProxy call_proxy(df, env) ;
    for( int i=0; i<nexpr; i++){
        Rcpp::checkUserInterrupt() ;

        env = dots.envir(i) ;
        call_proxy.set_env(env) ;

        SEXP call = args[dots.expr_index(i)] ;

        SEXP name = results_names[i] ;
        SEXP result = R_NilValue ;
        if( TYPEOF(call) == SYMSXP ){
            if(call_proxy.has_variable(call)){
                result = call_proxy.get_variable(PRINTNAME(call)) ;
            } else {
                result = shared_SEXP(env.find(CHAR(PRINTNAME(call)))) ;
            }
        } else if( TYPEOF(call) == LANGSXP ){
            call_proxy.set_call( args[dots.expr_index(i)] );

            // we need to protect the SEXP, that's what the Shelter does
            result = __( call_proxy.eval() ) ;

        } else if( Rf_length(call) == 1 ){
            boost::scoped_ptr<Gatherer> gather( constant_gatherer<DataFrame,LazySubsets>( call, df.nrows() ) );
            result = __( gather->collect() ) ;
        } else if( Rf_isNull(call)) {
            accumulator.rm(name) ;
            continue ;
        } else {
            stop( "cannot handle" ) ;
        }

        check_supported_type(result, name) ;

        if( Rf_length(result) == df.nrows() ){
            // ok
        } else if( Rf_length(result) == 1 ){
            // recycle
            Gatherer* gather = constant_gatherer<DataFrame,LazySubsets>( result, df.nrows() ) ;
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
    if( args.size() == 0 ) return df ;
    DataDots dots(env) ;
    if(is<RowwiseDataFrame>(df) ) {
        return mutate_grouped<RowwiseDataFrame, LazyRowwiseSubsets>( df, args, dots);
    } else if( is<GroupedDataFrame>( df ) ){
        return mutate_grouped<GroupedDataFrame, LazyGroupedSubsets>( df, args, dots);
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
    int n = Rf_length(x) ;
    if( n == 0 ) return wrap(0) ;
    SlicingIndex everything(0, n);
    boost::scoped_ptr<Result> res( count_distinct_result(x) );
    if( !res ){
        std::stringstream ss ;
        ss << "cannot handle object of type" << type2name(x) ;
        stop( ss.str() ) ;
    }
    return res->process(everything) ;
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

