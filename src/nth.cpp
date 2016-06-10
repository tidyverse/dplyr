#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

namespace dplyr {

    template <int RTYPE>
    class Nth : public Processor< RTYPE, Nth<RTYPE> > {
    public:
        typedef Processor< RTYPE, Nth<RTYPE> >  Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        Nth( Vector<RTYPE> data_, int idx_, STORAGE def_ = Vector<RTYPE>::get_na() ) :
            Base(data_),
            data(data_),
            idx(idx_),
            def(def_) {}

        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            if( n == 0 || idx > n || idx < -n ) return def ;
            int i = idx > 0 ? (idx -1) : (n+idx) ;
            return data[indices[i]] ;
        }

    private:
        Vector<RTYPE> data ;
        int idx ;
        STORAGE def ;
    } ;

    template <int RTYPE, int ORDER_RTYPE>
    class NthWith : public Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > {
    public:
        typedef Processor< RTYPE, NthWith<RTYPE, ORDER_RTYPE> > Base ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        NthWith( Vector<RTYPE> data_, int idx_, Vector<ORDER_RTYPE> order_, STORAGE def_ = Vector<RTYPE>::get_na() ) :
            Base(data_),
            data(data_),
            idx(idx_),
            order(order_),
            def(def_) {}

        inline STORAGE process_chunk( const SlicingIndex& indices ){
            int n = indices.size() ;
            if( n == 0 || idx > n || idx < -n) return def ;

            int i = idx > 0 ? (idx -1) : (n+idx) ;

            typedef VectorSliceVisitor<ORDER_RTYPE> Slice ;
            typedef OrderVectorVisitorImpl<ORDER_RTYPE,true,Slice> Visitor ;
            typedef Compare_Single_OrderVisitor<Visitor> Comparer ;

            Comparer comparer( Visitor( Slice(order, indices ) ) ) ;
            IntegerVector sequence = seq(0,n-1) ;
            std::nth_element( sequence.begin(), sequence.begin() + i, sequence.end(), comparer ) ;

            return data[ indices[ sequence[i] ] ] ;
        }

    private:
        Vector<RTYPE> data ;
        int idx ;
        Vector<ORDER_RTYPE> order ;
        STORAGE def ;
    } ;

}

template <int RTYPE>
Result* nth_noorder_default( Vector<RTYPE> data, int idx, Vector<RTYPE> def ){
     return new Nth<RTYPE>(data, idx, def[0] );
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
    stop("Unsupported vector type %s", Rf_type2char(TYPEOF(order))) ;
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
    stop("Unsupported vector type %s", Rf_type2char(TYPEOF(order))) ;
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
            stop( "could not find variable '%s'", CHAR(PRINTNAME(data)) );
        }
        data = subsets.get_variable(data) ;
    }

    tag = TAG(CDDR(call)) ;
    if( tag != R_NilValue && tag != Rf_install("n") ){
        stop( "the second argument of 'first' should be either 'n' or unnamed" ) ;
    }
    SEXP nidx = CADDR(call) ;
    if( ( TYPEOF(nidx) != REALSXP && TYPEOF(nidx) != INTSXP ) || LENGTH(nidx) != 1 ){
        // we only know how to handle the case where nidx is a length one
        // integer or numeric. In any other case, e.g. an expression for R to evaluate
        // we just fallback to R evaluation (#734)
        return 0 ;
    }
    int idx = as<int>(nidx) ;

    // easy case : just a single variable: first(x,n)
    if( nargs == 2 ){
        switch( TYPEOF(data) ){
        case INTSXP:  return new Nth<INTSXP>(data, idx) ;
        case REALSXP: return new Nth<REALSXP>(data, idx) ;
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
            if( TYPEOF(order_by) == SYMSXP && subsets.count(order_by) ){
                order_by = subsets.get_variable(order_by) ;

                switch( TYPEOF(data) ){
                    case LGLSXP:  return nth_with<LGLSXP>( data, idx, order_by ) ;
                    case INTSXP:  return nth_with<INTSXP>( data, idx, order_by ) ;
                    case REALSXP: return nth_with<REALSXP>( data, idx, order_by ) ;
                    case STRSXP:  return nth_with<STRSXP>( data, idx, order_by ) ;
                    default: break ;
                }
            }
            else {
                return 0;
            }


        } else {
            if( order_by == R_NilValue ){
                switch( TYPEOF(data) ){
                    case LGLSXP:  return nth_noorder_default<LGLSXP>(data, idx, def) ;
                    case INTSXP:  return nth_noorder_default<INTSXP>(data, idx, def) ;
                    case REALSXP: return nth_noorder_default<REALSXP>(data, idx, def) ;
                    case STRSXP:  return nth_noorder_default<STRSXP>(data, idx, def) ;
                    default: break ;
                }
            } else {
                if( TYPEOF(order_by) == SYMSXP && subsets.count(order_by) ){
                    order_by = subsets.get_variable(order_by) ;

                    switch( TYPEOF(data) ){
                        case LGLSXP:  return nth_with_default<LGLSXP>(data, idx, order_by, def) ;
                        case INTSXP:  return nth_with_default<INTSXP>(data, idx, order_by, def) ;
                        case REALSXP: return nth_with_default<REALSXP>(data, idx, order_by, def) ;
                        case STRSXP: return nth_with_default<STRSXP>(data, idx, order_by, def) ;
                        default: break ;
                    }
                }
                else {
                    return 0;
                }

            }
        }

    }
    stop("Unsupported vector type %s", Rf_type2char(TYPEOF(data))) ;
    return 0;
}

Result* firstlast_prototype( SEXP call, const LazySubsets& subsets, int nargs, int pos){
  SEXP tail = CDDR(call) ;

  SETCAR(call, Rf_install("nth")) ;

  Pairlist p(pos) ;
  if( Rf_isNull(tail) ){
    SETCDR(CDR(call), p)  ;
  } else {
    SETCDR(p, tail) ;
    SETCDR(CDR(call), p)  ;
  }
  Result* res = nth_prototype(call, subsets, nargs + 1) ;
  return res ;
}

Result* first_prototype( SEXP call, const LazySubsets& subsets, int nargs ){
  return firstlast_prototype(call, subsets, nargs, 1) ;
}

Result* last_prototype( SEXP call, const LazySubsets& subsets, int nargs ){
  return firstlast_prototype(call, subsets, nargs, -1) ;
}
