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

#ifndef dplyr_Result_CallbackProcessor_H
#define dplyr_Result_CallbackProcessor_H

namespace dplyr{
     
    // classes inherit from this template when they have a method with this signature
    // SEXP process_chunk( const std::vector<int>& indices)
    // 
    // the first time process_chunk is called, CallbackProcessor finds the right type
    // for storing the results, and it creates a suitable DelayedProcessor
    // object which is then used to fill the vector
    //
    // DelayedReducer and CallReducer are examples of how CallbackReducer is used
    //
    // it is assumed that the SEXP comes from evaluating some R expression, so
    // it should be one of a integer vector of length one, a numeric vector of 
    // length one or a character vector of length one
    template <typename CLASS>
    class CallbackProcessor : public Result {
    public:
        CallbackProcessor(){}
        virtual SEXP process( const ChunkIndexMap& map){
            Shelter<SEXP> __ ;
            ChunkIndexMap::const_iterator it = map.begin() ;
            CLASS* obj = static_cast<CLASS*>( this ) ;
            
            // first call, we don't know yet the type
            SEXP first_result = __( obj->process_chunk(it->second) );
            
            // get the appropriate Delayed Processor to handle it
            DelayedProcessor_Base<CLASS>* processor = get_delayed_processor<CLASS>(first_result) ;
            SEXP res = __( processor->delayed_process( map, first_result, obj ) ) ;
            delete processor ;
            
            return res ;        
        }
    } ;
    
}
#endif
