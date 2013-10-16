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

#ifndef dplyr_VisitorSetHasher_H
#define dplyr_VisitorSetHasher_H

namespace dplyr{

    template <typename VisitorSet>
    class VisitorSetHasher {
    public:
        VisitorSetHasher() : visitors(0){}
        
        VisitorSetHasher( VisitorSet* visitors_ ) : visitors(visitors_){} ;
        inline size_t operator()(int i) const {
            return visitors->hash(i) ;
        }
        
    private:
        VisitorSet* visitors ;  
    } ;
    
}

#endif
