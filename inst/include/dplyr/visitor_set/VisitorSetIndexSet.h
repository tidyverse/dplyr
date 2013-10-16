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

#ifndef dplyr_VisitorSetIndexSet_H
#define dplyr_VisitorSetIndexSet_H

namespace dplyr{
                  
    template <typename VisitorSet>
    class VisitorSetIndexSet : public boost::unordered_set<int, VisitorSetHasher<VisitorSet>, VisitorSetEqualPredicate<VisitorSet> > {
    private:
        typedef VisitorSetHasher<VisitorSet> Hasher ;
        typedef VisitorSetEqualPredicate<VisitorSet> EqualPredicate ;
        typedef boost::unordered_set<int, Hasher, EqualPredicate> Base ;
        
    public:
        VisitorSetIndexSet() : Base(){}
        
        VisitorSetIndexSet( VisitorSet& visitors_ ) : 
            Base( 1024, Hasher(&visitors_), EqualPredicate(&visitors_) )
        {}
        VisitorSetIndexSet( VisitorSet* visitors_ ) : 
            Base( 1024, Hasher(visitors_), EqualPredicate(visitors_) )
        {}
    } ;
    
}
#endif
