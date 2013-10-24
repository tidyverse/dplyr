#ifndef dplyr_tools_Shield_H
#define dplyr_tools_Shield_H

namespace Rcpp {
    
    template <typename T>
    class Shield{
    public:
        Shield( SEXP t_) : t(PROTECT(t_)){}
        ~Shield(){
            UNPROTECT(1) ;    
        }
        
        template <typename U>
        inline operator U() const { 
            return Rcpp::as<U>(t) ; 
        }
        
    private:    
        Shield( const Shield& ) ;
        Shield& operator=( const Shield& ) ;
    
        SEXP t ;
    } ;
    
}

#endif
