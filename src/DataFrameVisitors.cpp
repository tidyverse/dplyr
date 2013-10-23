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

#include <Rcpp.h>
#include <dplyr.h>

using namespace Rcpp ;

namespace dplyr {

    DataFrameVisitors::DataFrameVisitors( const DataFrame& data_, const CharacterVector& names) : 
        data(data_), 
        visitors()
    {
        std::string name ;
        int n = names.size() ;
        nvisitors = 0 ;
        std::vector<std::string> visitor_names_ ;
        for( int i=0; i<n; i++){
            name = (String)names[i] ;
            SEXP column = data[name] ;
            if( column != R_NilValue ){
                nvisitors++ ;
                visitor_names_.push_back( name ) ;
                visitors.push_back( visitor( column ) ) ;
            }
        }
        visitor_names = wrap( visitor_names_ );
    }
    
}
