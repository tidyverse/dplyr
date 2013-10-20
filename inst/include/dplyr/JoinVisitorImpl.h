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

#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

namespace dplyr{
        
    template <int RTYPE>
    class JoinVisitorImpl : public JoinVisitor, public comparisons<RTYPE>{
    public:
        typedef comparisons<RTYPE> Compare ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef boost::hash<STORAGE> hasher ;
    
        JoinVisitorImpl( Rcpp::Vector<RTYPE> left_, Rcpp::Vector<RTYPE> right_ ) : 
            left(left_), right(right_){}
          
        inline size_t hash(int i){
            return hash_fun( get(i) ) ; 
        }
        
        inline bool equal( int i, int j){
            return Compare::equal_or_both_na( get(i), get(j) ) ;
        }
        
    private:
        Rcpp::Vector<RTYPE> left, right ;
        hasher hash_fun ;
        
        inline STORAGE get(int i){
            return i>=0 ? left[i] : right[-i-1] ;    
        }
        
    } ;
    
}

#endif

