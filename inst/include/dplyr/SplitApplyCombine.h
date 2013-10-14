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

#ifndef dplyr_SplitApplyCombine_H
#define dplyr_SplitApplyCombine_H

namespace dplyr {

    class SplitApplyCombine {
    public:
        
        SplitApplyCombine( Rcpp::DataFrame data_ ) :
            data_frame(data_), 
            visitors(data_frame),
            results(), 
            trained(false)
        {}
        
        void group_by( Rcpp::CharacterVector groups ) ;
        
        void add_result( Rcpp::String name, Result* result ) ;
        
        void add_result( Rcpp::String name, Reducer_Proxy ) ;
        void add_reduce( Rcpp::String name, Rcpp::Function fun, Rcpp::String variable ) ;
        
        #define ADD_RESULT(__CLASS__,__FUN__)                                               \
        void add_result( Rcpp::String name, __CLASS__##_Proxy proxy ) ;                     \
        void add_##__FUN__( Rcpp::String name, Rcpp::String variable, bool na_rm = false )
             
        ADD_RESULT(Mean,mean);
        ADD_RESULT(Min,min)  ;
        ADD_RESULT(Max,max)  ;
        ADD_RESULT(Var,var)  ;
        ADD_RESULT(Sd,sd)    ;
        ADD_RESULT(Sum,sum)  ;
        
        #undef ADD_RESULT
        
        void add_count(Rcpp::String name ) ;
        
        Rcpp::List process() ;
            
        void train() ;
        
        void add_reduce_call( Rcpp::String target, Rcpp::Language call) ;
        
    private:
        Rcpp::DataFrame data_frame ;
        DataFrameVisitors visitors ;
        ResultSet results ;
        ChunkIndexMap index_map ;
        bool trained ;
    } ;
    
} // namespace dplyr

#endif
