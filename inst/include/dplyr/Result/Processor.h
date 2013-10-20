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

#ifndef dplyr_Result_Processor_H
#define dplyr_Result_Processor_H

namespace dplyr{
    
    // if we derive from this instead of deriving from Result, all we have to 
    // do is implement a process_chunk method that takes a Index_1_based& as
    // input and returns the suitable type (i.e. storage_type<OUTPUT>)
    // all the builtin result implementation (Mean, ...) use this. 
    template <int OUTPUT, typename CLASS>
    class Processor : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<OUTPUT>::type STORAGE ;
         
        Processor(){}
        
        virtual SEXP process(const Rcpp::GroupedDataFrame& gdf ) {
            int n = gdf.ngroups() ; 
            Rcpp::Shield<SEXP> res( Rf_allocVector( OUTPUT, n) );
            STORAGE* ptr = Rcpp::internal::r_vector_start<OUTPUT>(res) ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++)
                ptr[i] = obj->process_chunk(gdf.group(i)) ;
            return res ;        
        }
        
        virtual SEXP process( const Rcpp::FullDataFrame& df){
            CLASS* obj = static_cast<CLASS*>(this) ;
            return Rcpp::Vector<OUTPUT>( obj->process_chunk(df.get_index()) );    
        }
    } ;
    
    template <typename CLASS>
    class Processor<STRSXP, CLASS> : public Result {
    public:
        Processor(){}
        
        virtual SEXP process(const Rcpp::GroupedDataFrame& gdf) {
            int n = gdf.ngroups() ; 
            Rcpp::Shield<SEXP> res( Rf_allocVector( STRSXP, n) ) ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++)
                SET_STRING_ELT( res, i, obj->process_chunk(gdf.group(i)) );
            return res ;        
        }
        
        virtual SEXP process( const Rcpp::FullDataFrame& df){
            CLASS* obj = static_cast<CLASS*>(this) ;
            return Rf_mkString( obj->process_chunk(df.get_index()) );    
        }
        
    } ;

}
#endif
