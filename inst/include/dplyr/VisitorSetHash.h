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

#ifndef dplyr_VisitorSetHash_H
#define dplyr_VisitorSetHash_H

namespace dplyr{
           
template <typename Class>
class VisitorSetHash {
public:
    size_t hash( int j) const {
        const Class& obj = static_cast<const Class&>(*this) ; 
        size_t seed = 0 ;
        int n = obj.size() ;
        for( int k=0; k<n; k++){
            boost::hash_combine( seed, obj.get(k)->hash(j) ) ;
        }
        return seed ;
    }
} ;
    
}

#endif
