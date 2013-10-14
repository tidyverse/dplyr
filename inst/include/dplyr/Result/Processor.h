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
    // do is implement a process_chunk method that takes a std::vector<int>& as
    // input and returns the suitable type (i.e. storage_type<OUTPUT>)
    // all the builtin result implementation (Mean, ...) use this. 
    template <int OUTPUT, typename CLASS>
    class Processor : public Result {
    public:
        typedef typename Rcpp::traits::storage_type<OUTPUT>::type STORAGE ;
         
        Processor(){}
        
        virtual SEXP process(const ChunkIndexMap& map ) {
            Shelter<SEXP> __ ;
            int n = map.size() ; 
            SEXP res = __( Rf_allocVector( OUTPUT, n) ) ;
            STORAGE* ptr = Rcpp::internal::r_vector_start<OUTPUT>(res) ;
            ChunkIndexMap::const_iterator it = map.begin() ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++, ++it)
                ptr[i] = obj->process_chunk(it->second ) ;
            return res ;        
        }
    } ;
    
    template <typename CLASS>
    class Processor<STRSXP, CLASS> : public Result {
    public:
        Processor(){}
        
        virtual SEXP process(const ChunkIndexMap& map) {
            int n = map.size() ; 
            Shield<SEXP> res( Rf_allocVector( STRSXP, n) ) ;
            ChunkIndexMap::const_iterator it = map.begin() ;
            CLASS* obj = static_cast<CLASS*>(this) ;
            for( int i=0; i<n; i++, ++it)
                SET_STRING_ELT( res, i, obj->process_chunk(it->second ) );
            return res ;        
        }
        
    } ;

}
#endif
