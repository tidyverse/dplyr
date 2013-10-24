#ifndef dplyr_tools_Armor_H
#define dplyr_tools_Armor_H

namespace Rcpp {
    
    template <typename T>
    class Armor {
    public:
        
        Armor() : data(){
            init(R_NilValue) ;    
        }
        
        template <typename U>
        Armor( U x ) : data() {
            init( Rcpp::wrap(x) ) ;
        }
        
        inline operator SEXP(){
            return data ;
        }
              
        template <typename U>
        inline Armor& operator=( U x ){
            REPROTECT(data = Rcpp::wrap(x), index) ;
            return *this ;
        }
        
        ~Armor(){
            UNPROTECT(1) ;
        }
        
    private:
        void init(SEXP x){
           PROTECT_WITH_INDEX( data = x, &index ) ;     
        }
        
        SEXP data ;
        PROTECT_INDEX index ;
        
        Armor(const Armor&) ;
        Armor& operator=(const Armor&) ; 
    } ;
}

#endif
