#ifndef dplyr_Subset_H
#define dplyr_Subset_H

namespace dplyr {
    
    class Subset {
    public:
        Subset(){} ;
        virtual ~Subset(){} ;
        virtual SEXP get( const Index_0_based& indices ) const = 0 ;
        virtual SEXP get( const Everything& ) const = 0 ;
    } ;
    
    template <int RTYPE>
    class SubsetTemplate : public Subset {
    public:
        SubsetTemplate( SEXP x ) : object(x){}
        virtual SEXP get( const Index_0_based& indices ) const {
            return wrap_subset<RTYPE>( object, indices ) ;    
        }
        virtual SEXP get( const Everything& ) const { return object ; }
    private:
        SEXP object ;
    } ;
    
    inline Subset* subset(SEXP x){
        switch( TYPEOF(x) ){
            case INTSXP: return new SubsetTemplate<INTSXP>(x) ;
            case REALSXP: return new SubsetTemplate<REALSXP>(x) ;
            case STRSXP: return new SubsetTemplate<STRSXP>(x) ;
        }
        return 0 ;
    }
    
}

#endif
