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

#ifndef dplyr_DataFrameVisitors_set_H
#define dplyr_DataFrameVisitors_set_H

namespace dplyr {
 
    class DataFrameVisitorsIndexSet : public boost::unordered_set<int, VisitorSetHasher<DataFrameVisitors>, VisitorSetEqualPredicate<DataFrameVisitors> > {
    private:
        typedef VisitorSetHasher<DataFrameVisitors> Hasher ;
        typedef VisitorSetEqualPredicate<DataFrameVisitors> EqualPredicate ;
        typedef boost::unordered_set<int, Hasher, EqualPredicate> Base ;
        
    public:
        DataFrameVisitorsIndexSet() : Base(){}
        
        DataFrameVisitorsIndexSet( DataFrameVisitors& visitors_ ) : 
            Base( 1024, Hasher(&visitors_), EqualPredicate(&visitors_) )
        {}
        DataFrameVisitorsIndexSet( DataFrameVisitors* visitors_ ) : 
            Base( 1024, Hasher(visitors_), EqualPredicate(visitors_) )
        {}
    } ;
    
}

#endif
