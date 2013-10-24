#ifndef dplyr_tools_Shelter_H
#define dplyr_tools_Shelter_H

namespace Rcpp {
    
    template <class T>
    class Shelter {
    public:
        Shelter() : nprotected(0){}
        
        inline SEXP operator()(SEXP x){
            nprotected++;             
            return PROTECT(x) ;
        }
        
        ~Shelter(){
            UNPROTECT(nprotected) ;
            nprotected = 0 ;
        }
        
    private:
        int nprotected ;
        
        Shelter(const Shelter&) ;
        Shelter& operator=(const Shelter&) ; 
    } ;
}

#endif
