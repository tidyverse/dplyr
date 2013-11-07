#ifndef dplyr_registration_H
#define dplyr_registration_H

#if !defined(COMPILING_DPLYR)

#define GRAB_CALLABLE(__FUN__) static Fun fun = (Fun)R_GetCCallable( "dplyr", #__FUN__ ) ;

inline DataFrame build_index_cpp( DataFrame data ){
    typedef DataFrame (*Fun)(DataFrame) ;
    GRAB_CALLABLE(build_index_cpp)
    return fun(data) ;
}

inline void registerResult( const char* name, ResultPrototype proto){
    typedef void (*Fun)(const char*, ResultPrototype ) ;
    GRAB_CALLABLE(registerResult)
    return fun(name, proto) ;
}

#endif

#endif

