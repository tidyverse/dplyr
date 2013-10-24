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
    
    Subset* subset( SEXP x ) ; 

}

#endif
