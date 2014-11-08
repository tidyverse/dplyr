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
    class StringOrderVisitor : public OrderVisitor {
    public:
        typedef OrderVectorVisitorImpl<INTSXP, ascending, IntegerVector> IntegerOrderVisitor ; 
        
        StringOrderVisitor( const CharacterVector& data_ ) : data(data_), visitor(orders(data)) {}
        
        inline bool equal(int i, int j) const {
            return visitor.equal(i,j) ;    
        }
        
        inline bool before(int i, int j) const {
            return visitor.before(i,j) ;    
        }
        
        inline SEXP get() {
            return data ;    
        }
        
    private:
        
        
        inline IntegerVector orders( const CharacterVector strings ) {
            Language call( "match", data, Language("sort", data) ) ;
            return Rf_eval( call, R_GlobalEnv ) ;
        }
        
        CharacterVector data ;
        IntegerOrderVisitor visitor ;
        
    } ;

    inline OrderVisitor* order_visitor( SEXP vec, bool ascending ){
        if( ascending ){
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , true, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, true, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , true, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new StringOrderVisitor<true>( vec ) ;
                case CPLXSXP:  return new OrderVectorVisitorImpl<CPLXSXP , true, Vector<CPLXSXP > >( vec ) ;
                default: break ;
            }
        } else { 
            switch( TYPEOF(vec) ){
                case INTSXP:  return new OrderVectorVisitorImpl<INTSXP , false, Vector<INTSXP > >( vec ) ;
                case REALSXP: return new OrderVectorVisitorImpl<REALSXP, false, Vector<REALSXP> >( vec ) ;
                case LGLSXP:  return new OrderVectorVisitorImpl<LGLSXP , false, Vector<LGLSXP > >( vec ) ;
                case STRSXP:  return new StringOrderVisitor<false>( vec ) ;
                case CPLXSXP:  return new OrderVectorVisitorImpl<CPLXSXP , false, Vector<CPLXSXP > >( vec ) ;
                default: break ;
            }
        }
        
        // should not happen
        return 0 ;
    }
}

#endif
