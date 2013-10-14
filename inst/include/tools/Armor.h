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

#ifndef dplyr_tools_Armor_H
#define dplyr_tools_Armor_H

namespace dplyr {
    
    template <typename T>
    class Armor {
    public:
        
        Armor() : data(){
            init(R_NilValue) ;    
        }
        
        template <typename U>
        Armor( U x ) : data() {
            init( Rcpp::wrap(x) ) ;
        }
        
        inline operator SEXP(){
            return data ;
        }
              
        template <typename U>
        inline Armor& operator=( U x ){
            REPROTECT(data = Rcpp::wrap(x), index) ;
            return *this ;
        }
        
        ~Armor(){
            UNPROTECT(1) ;
        }
        
    private:
        void init(SEXP x){
           PROTECT_WITH_INDEX( data = x, &index ) ;     
        }
        
        SEXP data ;
        PROTECT_INDEX index ;
        
        Armor(const Armor&) ;
        Armor& operator=(const Armor&) ; 
    } ;
}

#endif
