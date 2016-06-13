#ifndef dplyr_Result_factories_H
#define dplyr_Result_factories_H

namespace dplyr {

    inline Count* count(){ return new Count ; }

    class Reducer_Proxy{
    public:
        Reducer_Proxy( Rcpp::Function fun_, Rcpp::String variable_ ):
            fun(fun_), variable(variable_)
        {}
        Rcpp::Function fun ;
        Rcpp::String variable ;
    } ;

    inline Reducer_Proxy reduce( Rcpp::Function fun, Rcpp::String variable){
        return Reducer_Proxy( fun, variable ) ;
    }

    #define MAKE_PROXY(PREFIX,_prefix_)                                         \
    class PREFIX##_Proxy {                                                      \
    public:                                                                     \
        PREFIX##_Proxy( Rcpp::String variable_, bool na_rm_ ) :                 \
            variable(variable_), na_rm(na_rm_){}                                \
        Rcpp::String variable ;                                                 \
        bool na_rm ;                                                            \
    } ;                                                                         \
    inline PREFIX##_Proxy _prefix_( Rcpp::String variable, bool na_rm = false ){\
        return PREFIX##_Proxy( variable, na_rm ) ;                              \
    }

    MAKE_PROXY(Mean,mean)
    MAKE_PROXY(Sum,sum)
    MAKE_PROXY(Min,min)
    MAKE_PROXY(Max,max)
    MAKE_PROXY(Var,var)
    MAKE_PROXY(Sd,sd)

    #undef MAKE_PROXY
}

#endif
