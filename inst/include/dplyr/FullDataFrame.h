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

#ifndef dplyr_tools_FullDataFrame_H
#define dplyr_tools_FullDataFrame_H

namespace Rcpp {
    
    class FullDataFrame {
    public:
        FullDataFrame( const DataFrame& data_ ) : 
            index_data( seq(0, data_.nrows()-1 )), index(index_data) {}
         
        const Index_0_based& get_index() const { 
            return index ; 
        }    
            
    private:
        IntegerVector index_data ;
        Index_0_based index ;
    } ;
    
}
#endif              
