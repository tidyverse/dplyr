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
 
    class DataFrameVisitorsIndexSet : public boost::unordered_set<int, DataFrameVisitorsHasher, DataFrameVisitorsEqual> {
    private:
        typedef boost::unordered_set<int, DataFrameVisitorsHasher, DataFrameVisitorsEqual> Base ;
    
    public:
        DataFrameVisitorsIndexSet() : Base(){}
        
        DataFrameVisitorsIndexSet( DataFrameVisitors& visitors_ ) : 
            Base( 1024, DataFrameVisitorsHasher(&visitors_), DataFrameVisitorsEqual(&visitors_) )
        {}
        DataFrameVisitorsIndexSet( DataFrameVisitors* visitors_ ) : 
            Base( 1024, DataFrameVisitorsHasher(visitors_), DataFrameVisitorsEqual(visitors_) )
        {}
    } ;
    
}

#endif
