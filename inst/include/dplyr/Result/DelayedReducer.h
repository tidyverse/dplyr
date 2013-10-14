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

#ifndef dplyr_DelayedReducer_H
#define dplyr_DelayedReducer_H

namespace dplyr {
    
    template <int INPUT_RTYPE>
    class DelayedReducer : public CallbackProcessor< DelayedReducer<INPUT_RTYPE> > {
        public:
            DelayedReducer(Rcpp::Function fun_, Rcpp::String variable_, SEXP data_ ): 
                call(fun_), proxy(call, 1), data(data_) {}
            
            virtual ~DelayedReducer(){} ;
            
            inline SEXP process_chunk( const std::vector<int>& indices){
                proxy = wrap_subset<INPUT_RTYPE>( data, indices );
                return call.fast_eval() ;
            }
            
        private:
            
            Rcpp::Language call ;
            Rcpp::Language::Proxy proxy ;
            SEXP data ;
    } ;

} // namespace dplyr

#endif
