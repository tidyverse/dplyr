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

    inline OrderVisitor* order_visitor( SEXP vec, bool ascending ){
        if( ascending )
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , true, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, true, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , true, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , true, Vector<STRSXP > >( vec ) ;
                default: break ;
            }
        else 
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , false, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, false, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , false, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new OrderVectorVisitorImpl<STRSXP , false, Vector<STRSXP > >( vec ) ;
                default: break ;
            }
        
        // should not happen
        return 0 ;
    }
}

#endif
