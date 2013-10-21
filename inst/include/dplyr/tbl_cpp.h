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

#ifndef dplyr_tools_tbl_cpp_H
#define dplyr_tools_tbl_cpp_H

namespace dplyr {
    
    template <typename Df>
    inline void set_rownames( Df& data, int n ){
        data.attr( "row.names" ) = Rcpp::IntegerVector::create( 
            Rcpp::IntegerVector::get_na(), -n) ;   
    }
    
    inline Rcpp::CharacterVector classes_grouped(){
        return Rcpp::CharacterVector::create( "grouped_cpp", "tbl_cpp", "tbl", "data.frame") ;        
    }
    inline Rcpp::CharacterVector classes_not_grouped(){
        return Rcpp::CharacterVector::create( "tbl_cpp", "tbl", "data.frame") ;
    }

    class tbl_cpp{
    public:
        tbl_cpp( Rcpp::List data_, Rcpp::CharacterVector names, int nr ) : data(data_){
            data.names() = names ;
            set_rownames(data, nr ) ;
            data.attr( "class" ) = classes_not_grouped()  ;
        }
        
        inline operator SEXP(){ 
            return data ; 
        }
        
    private:
        List data ;
    } ;
    
    class summarised_grouped_tbl_cpp{
    public:
        summarised_grouped_tbl_cpp( Rcpp::List data_, Rcpp::CharacterVector names, const GroupedDataFrame& source ) : data(data_){
            int nr = source.ngroups() ;
            data.names() = names ;
            set_rownames(data, nr ) ;
            data.attr( "class" ) = classes_grouped()  ;
            data.attr( "vars") = source.attr("vars") ;
            data.attr( "labels" ) = source.attr("labels" );
            data.attr( "index") = get_index(nr) ;
        }
        
        inline operator SEXP(){ 
            return data ; 
        }
        
    private:
        
        List get_index(int n){
            List index(n) ;
            for( int i=0; i<n; i++) index[i] = i ;
            return index ;
        }
        
        List data ;
    } ;
    
}

#endif
