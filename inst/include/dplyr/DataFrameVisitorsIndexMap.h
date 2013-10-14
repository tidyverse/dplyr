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

#ifndef dplyr_DataFrameVisitors_map_H
#define dplyr_DataFrameVisitors_map_H

namespace dplyr {

    template <typename VALUE>
    class DataFrameVisitorsIndexMap : 
        public boost::unordered_map<int, VALUE, VisitorSetHasher<DataFrameVisitors> , DataFrameVisitorsEqual> {
    private:
        typedef VisitorSetHasher<DataFrameVisitors> Hasher ;
        typedef typename boost::unordered_map<int, VALUE, Hasher, DataFrameVisitorsEqual> Base ;
        
    public:
        DataFrameVisitorsIndexMap() : Base(), visitors(0) {}
                   
        DataFrameVisitorsIndexMap( DataFrameVisitors& visitors_ ) : 
            Base( 1024, Hasher(&visitors_), DataFrameVisitorsEqual(&visitors_) ), 
            visitors(&visitors_)
        {}
        
        DataFrameVisitorsIndexMap( DataFrameVisitors* visitors_ ) : 
            Base( 1024, Hasher(visitors_), DataFrameVisitorsEqual(visitors_) ), 
            visitors(visitors_)
        {}
        
        DataFrameVisitors* visitors ;
    
    } ;
      
    typedef DataFrameVisitorsIndexMap< std::vector<int> > ChunkIndexMap ; 
    
}

#endif
