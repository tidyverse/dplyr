#ifndef dplyr_registration_H
#define dplyr_registration_H

#if !defined(COMPILING_DPLYR)

#define GRAB_CALLABLE(__FUN__) static Fun fun = (Fun)R_GetCCallable( "dplyr", #__FUN__ ) ;

inline DataFrame build_index_cpp( DataFrame data ){
    typedef DataFrame (*Fun)(DataFrame) ;
    GRAB_CALLABLE(build_index_cpp)
    return fun(data) ;
}

inline void registerHybridHandler( const char* name, HybridHandler proto){
    typedef void (*Fun)(const char*, HybridHandler ) ;
    GRAB_CALLABLE(registerHybridHandler)
    return fun(name, proto) ;
}

#endif

#endif

