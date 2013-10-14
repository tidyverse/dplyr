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

#ifndef dplyr_Subset_H
#define dplyr_Subset_H

namespace dplyr {
    
    class Subset {
    public:
        Subset(){} ;
        virtual ~Subset(){} ;
        virtual SEXP get( const std::vector<int>& indices ) const = 0 ;
        virtual SEXP get( const Rcpp::IntegerVector& indices ) const = 0 ;
        virtual SEXP get() const = 0 ;
    } ;
    
    template <int RTYPE>
    class SubsetTemplate : public Subset {
    public:
        SubsetTemplate( SEXP x ) : object(x){}
        virtual SEXP get( const std::vector<int>& indices ) const {
            return wrap_subset<RTYPE>( object, indices ) ;    
        }
        virtual SEXP get( const Rcpp::IntegerVector& indices ) const {
            return wrap_subset_1_based<RTYPE>( object, indices ) ;    
        }
        virtual SEXP get() const { return object ; }
    private:
        SEXP object ;
    } ;
    
    Subset* subset( SEXP x ) ; 

}

#endif
