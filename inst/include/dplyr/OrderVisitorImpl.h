#ifndef dplyr_OrderVectorVisitor_Impl_H
#define dplyr_OrderVectorVisitor_Impl_H

namespace dplyr {

    // version used for ascending = true
    template <int RTYPE, bool ascending, typename VECTOR>
    class OrderVectorVisitorImpl : public OrderVisitor, public comparisons<RTYPE> {
        typedef comparisons<RTYPE> compare ;
    public:
        /**
         * The type of data : int, double, SEXP, Rcomplex
         */
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        OrderVectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}

        inline bool equal(int i, int j) const {
            return compare::equal_or_both_na( vec[i], vec[j] ) ;
        }

        inline bool before(int i, int j) const {
            return compare::is_less( vec[i], vec[j] ) ;
        }

        SEXP get(){ return vec ; }

    private:
        VECTOR vec ;
    } ;

    // version used for ascending = false
    template <int RTYPE, typename VECTOR>
    class OrderVectorVisitorImpl<RTYPE,false, VECTOR> : public OrderVisitor, public comparisons<RTYPE> {
        typedef comparisons<RTYPE> compare ;
    public:

        /**
         * The type of data : int, double, SEXP, Rcomplex
         */
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        OrderVectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}

        inline bool equal(int i, int j) const {
            return compare::equal_or_both_na(vec[i], vec[j]) ;
        }

        inline bool before(int i, int j) const {
            return compare::is_greater( vec[i], vec[j] ) ;
        }

        SEXP get(){ return vec ; }

    private:
        VECTOR vec ;
    } ;

    template <bool ascending>
    class OrderCharacterVectorVisitorImpl : public OrderVisitor {
    public:
      OrderCharacterVectorVisitorImpl( const CharacterVector& vec_ ) :
        vec(vec_),
        orders( CharacterVectorOrderer(vec).get() )
      {}

      inline bool equal(int i, int j) const {
        return orders.equal(i,j) ;
      }

      inline bool before( int i, int j) const{
        return orders.before(i,j);
      }

      SEXP get(){ return vec; }

    private:
      CharacterVector vec ;
      OrderVectorVisitorImpl<INTSXP, ascending, IntegerVector> orders ;
    } ;

    // ---------- data frame columns

    // ascending = true
    template <bool ascending>
    class OrderVisitorDataFrame : public OrderVisitor {
    public:
        OrderVisitorDataFrame( const DataFrame& data_ ) : data(data_), visitors(data) {}

        inline bool equal( int i, int j) const {
            return visitors.equal(i,j) ;
        }

        inline bool before( int i, int j) const {
            return visitors.less(i,j) ;
        }

        inline SEXP get(){
            return data ;
        }

    private:
        DataFrame data ;
        DataFrameVisitors visitors ;
    } ;

    template <>
    class OrderVisitorDataFrame<false> : public OrderVisitor{
    public:
        OrderVisitorDataFrame( const DataFrame& data_ ) : data(data_), visitors(data) {}

        inline bool equal( int i, int j) const {
            return visitors.equal(i,j) ;
        }

        inline bool before( int i, int j) const {
            return visitors.greater(i,j) ;
        }

        inline SEXP get(){
            return data ;
        }

    private:
        DataFrame data ;
        DataFrameVisitors visitors ;
    } ;

    // ---------- matrix columns

    // ascending = true
    template <int RTYPE, bool ascending>
    class OrderVisitorMatrix : public OrderVisitor {
    public:
        OrderVisitorMatrix( const Matrix<RTYPE>& data_ ) : data(data_), visitors(data) {}

        inline bool equal( int i, int j) const {
            return visitors.equal(i,j) ;
        }

        inline bool before( int i, int j) const {
            return visitors.less(i,j) ;
        }

        inline SEXP get(){
            return data ;
        }

    private:
        Matrix<RTYPE> data ;
        MatrixColumnVisitor<RTYPE> visitors ;
    } ;

    // ascending = false
    template <int RTYPE>
    class OrderVisitorMatrix<RTYPE, false> : public OrderVisitor {
    public:
        OrderVisitorMatrix( const Matrix<RTYPE>& data_ ) : data(data_), visitors(data) {}

        inline bool equal( int i, int j) const {
            return visitors.equal(i,j) ;
        }

        inline bool before( int i, int j) const {
            return visitors.greater(i,j) ;
        }

        inline SEXP get(){
            return data ;
        }

    private:
        Matrix<RTYPE> data ;
        MatrixColumnVisitor<RTYPE> visitors ;
    } ;


    inline OrderVisitor* order_visitor( SEXP vec, bool ascending ){
        if( Rf_isMatrix(vec) ){
            if(ascending) {
                switch( TYPEOF(vec) ){
                    case INTSXP:   return new OrderVisitorMatrix<INTSXP  , true>( vec ) ;
                    case REALSXP:  return new OrderVisitorMatrix<REALSXP , true>( vec ) ;
                    case LGLSXP:   return new OrderVisitorMatrix<LGLSXP  , true>( vec ) ;
                    case STRSXP:   return new OrderVisitorMatrix<STRSXP  , true>( vec ) ;
                    case CPLXSXP:  return new OrderVisitorMatrix<CPLXSXP , true>( vec ) ;
                    default: break ;
                }
            } else {
                switch( TYPEOF(vec) ){
                    case INTSXP:   return new OrderVisitorMatrix<INTSXP  , false>( vec ) ;
                    case REALSXP:  return new OrderVisitorMatrix<REALSXP , false>( vec ) ;
                    case LGLSXP:   return new OrderVisitorMatrix<LGLSXP  , false>( vec ) ;
                    case STRSXP:   return new OrderVisitorMatrix<STRSXP  , false>( vec ) ;
                    case CPLXSXP:  return new OrderVisitorMatrix<CPLXSXP , false>( vec ) ;
                    default: break ;
                }
            }
            stop( "unimplemented matrix type" ) ;
            return 0 ;
        }

        if( ascending ){
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , true, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, true, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , true, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new OrderCharacterVectorVisitorImpl<true>( vec ) ;
                case CPLXSXP:  return new OrderVectorVisitorImpl<CPLXSXP , true, Vector<CPLXSXP > >( vec ) ;
                case VECSXP:
                    {
                        if( Rf_inherits( vec, "data.frame" ) ){
                            return new OrderVisitorDataFrame<true>( vec ) ;
                        }
                        break ;
                    }
                default: break ;
            }
        } else {
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , false, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, false, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , false, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new OrderCharacterVectorVisitorImpl<false>( vec ) ;
                case CPLXSXP:  return new OrderVectorVisitorImpl<CPLXSXP , false, Vector<CPLXSXP > >( vec ) ;
                case VECSXP:
                {
                    if( Rf_inherits( vec, "data.frame" ) ){
                        return new OrderVisitorDataFrame<false>( vec ) ;
                    }
                    break ;
                }

                default: break ;
            }
        }

        // should not happen
        return 0 ;
    }
}

#endif
