// Copyright (C) 2013    Romain Francois
// Copyright (C) 2013    Rice University
//
// This file is part of dplyr.
//
// dplyr is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// dplyr is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with dplyr.  If not, see <http://www.gnu.org/licenses/>.

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
