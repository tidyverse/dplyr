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

#ifndef dplyr_VisitorSetLess_H
#define dplyr_VisitorSetLess_H

namespace dplyr{

template <typename Class>
class VisitorSetLess {
public:
    bool less( int i, int j) const {
        if( i == j ) return false ;
        const Class& obj = static_cast<const Class&>(*this) ; 
        int n=obj.size();
        for( int k=0; k<n; k++){
            typename Class::visitor_type* visitor = obj.get(k) ; 
            if( ! visitor->equal(i,j) ){
                return visitor->less(i,j) ;
            } 
        }
        // if we end up here, it means rows i and j are equal
        // we break the tie using the indices
        return i < j ;
    }
} ;
    
}

#endif
