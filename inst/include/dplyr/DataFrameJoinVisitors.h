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

#ifndef dplyr_DataFrameJoinVisitors_H
#define dplyr_DataFrameJoinVisitors_H

namespace dplyr{
    
    class DataFrameJoinVisitors : 
        public VisitorSetEqual<DataFrameJoinVisitors>, 
        public VisitorSetHash<DataFrameJoinVisitors>
    {
    public:
        
        DataFrameJoinVisitors(const Rcpp::DataFrame& left_, const Rcpp::DataFrame& right_, Rcpp::CharacterVector names) : 
            left(left_), right(right_), nvisitors(names.size()), visitors(nvisitors)
        {    
            std::string name ;
            for( int i=0; i<nvisitors; i++){
                name = names[i] ;
                visitors[i] = join_visitor( left[name], right[name]) ;
            }
        }
        
        ~DataFrameJoinVisitors(){
            delete_all(visitors);    
        }
        
        inline JoinVisitor* get(int k) const { return visitors[k] ; }
        inline int size() const{ return nvisitors ; } 
        
    private:
        const Rcpp::DataFrame& left ;
        const Rcpp::DataFrame& right ;
        int nvisitors ;
        std::vector<JoinVisitor*> visitors ;
        
    } ;
    
}

#endif

