#ifndef dplyr_CallProxy_H
#define dplyr_CallProxy_H

namespace dplyr {
       
    class CallElementProxy {
    public:
        CallElementProxy(SEXP symbol_, SEXP object_) : symbol(symbol_), object(object_){}
        
        inline void set(SEXP value){ 
            SETCAR(object, value) ;
        } 
        
        SEXP symbol;
        SEXP object;
    } ;

}
