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

#ifndef dplyr_Result_ResultSet_H
#define dplyr_Result_ResultSet_H

namespace dplyr {
    
    class ResultSet {
    public:
        ResultSet( ) : results(), names(), n(0) {}
        
        void add_result( const std::string& name, Result* result ){
            results.push_back( result ) ;
            names.push_back( name ) ;
            n++ ;
        }
        
        ~ResultSet(){
            delete_all( results ) ;
        }
        
        Result* get(int k){ return results[k] ; }
        inline int size() const { return n ; }
        Rcpp::String name(int k) const { return names[k] ; } 
        
    private:
        std::vector<Result*> results ;
        std::vector<std::string> names ;
        int n ;
    } ;
    
}

#endif
