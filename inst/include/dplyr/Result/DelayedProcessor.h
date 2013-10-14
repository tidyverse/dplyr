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

#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

namespace dplyr{
    
    template <typename CLASS>
    class DelayedProcessor_Base {
       public:
           DelayedProcessor_Base(){}
           virtual ~DelayedProcessor_Base(){}
           
           virtual SEXP delayed_process( const ChunkIndexMap& map, SEXP first_result, CLASS* ) = 0;
    } ;
    
    template <int RTYPE, typename CLASS>
    class DelayedProcessor : public DelayedProcessor_Base<CLASS> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
         
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const ChunkIndexMap& map, SEXP first_result, CLASS* obj) {
            Shelter<SEXP> __ ;
            int n = map.size() ; 
            SEXP res = __( Rf_allocVector( RTYPE, n) ) ;
            STORAGE* ptr = Rcpp::internal::r_vector_start<RTYPE>(res) ;
            ptr[0] = Rcpp::as<STORAGE>( first_result );
            ChunkIndexMap::const_iterator it = map.begin() ; ++it ;
            for( int i=1; i<n; i++, ++it)
                ptr[i] = Rcpp::as<STORAGE>( obj->process_chunk(it->second ) ) ;
            return res ;        
        }
                      
    } ;
    
    template <typename CLASS>
    class DelayedProcessor<STRSXP, CLASS> : public DelayedProcessor_Base<CLASS> {
    public:
        DelayedProcessor(){}
        
        virtual SEXP delayed_process(const ChunkIndexMap& map, SEXP first_result, CLASS* obj) {
            Shelter<SEXP> __ ;
            int n = map.size() ; 
            SEXP res = __( Rf_allocVector( STRSXP, n) ) ;
            SET_STRING_ELT( res, 0, STRING_ELT(first_result, 0 ) ) ;
            ChunkIndexMap::const_iterator it = map.begin() ; ++it ;
            for( int i=1; i<n; i++, ++it)
                SET_STRING_ELT( res, i, STRING_ELT( obj->process_chunk(it->second ), 0) ) ;
            return res ;        
        }
        
    } ;
    
    template <typename CLASS>
    DelayedProcessor_Base<CLASS>* get_delayed_processor(SEXP first_result){
        if( Rcpp::is<int>( first_result ) ){       
            return new DelayedProcessor<INTSXP, CLASS>() ;    
        } else if( Rcpp::is<double>( first_result) ){
            return new DelayedProcessor<REALSXP, CLASS>() ;    
        } else if( Rcpp::is<Rcpp::String>( first_result) ){
            return new DelayedProcessor<STRSXP, CLASS>() ;
        }
        return 0 ;
    }

}
#endif
