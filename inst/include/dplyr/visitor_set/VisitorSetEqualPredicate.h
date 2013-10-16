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

#ifndef dplyr_VisitorSetEqualPredicate_H
#define dplyr_VisitorSetEqualPredicate_H

namespace dplyr{

    template <typename VisitorSet>
    class VisitorSetEqualPredicate {
    public:
        VisitorSetEqualPredicate() : visitors(0){}
        
        VisitorSetEqualPredicate( VisitorSet* visitors_ ) : visitors(visitors_) {} ;
        inline bool operator()(int i, int j) const {
            return visitors->equal(i,j) ;
        }
        
    private:
        VisitorSet* visitors ;  
    } ;

}

#endif
