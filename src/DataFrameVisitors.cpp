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

    DataFrameVisitors::DataFrameVisitors( const DataFrame& data_ ) : 
        data(data_), 
        visitors(), 
        visitor_names(), 
        nvisitors(0)
    {}
    
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
    
    DataFrameVisitors& DataFrameVisitors::add_visitor( Rcpp::String column ){
        SEXP vec = data[ column ] ;
        if( ! Rf_isNull( vec ) ){
            nvisitors++ ;
            visitors.push_back( visitor(vec) ) ;
            visitor_names.push_back( column ) ;
        }
        return *this ;
    }
    
    DataFrameVisitors::~DataFrameVisitors(){
        delete_all( visitors );
    }
       
   /**
     * Creates a data frame by indexing the visited vectors. 
     * Similar to df[ index, ]
     *
     * This will go in DataFrame later
     */
    DataFrame DataFrameVisitors::copy( const IntegerVector& index ) const {
         int nrows = index.size() ;
         List out(nvisitors);
         for( int k=0; k<nvisitors; k++){
            out[k] = visitors[k]->copy(index) ;    
         }
         out.attr("class") = "data.frame" ;
         out.attr("row.names") = IntegerVector::create( 
             IntegerVector::get_na(), -nrows
         ) ;
         out.names() = visitor_names ;
         return out.asSexp() ;
    }
    
    DataFrame DataFrameVisitors::subset( const LogicalVector& index ) const {
         int nrows = sum( index ) ;
         List out(nvisitors);
         for( int k=0; k<nvisitors; k++){
            out[k] = visitors[k]->subset(index, nrows) ;    
         }
         out.attr("class") = "data.frame" ;
         out.attr("row.names") = IntegerVector::create( 
             IntegerVector::get_na(), -nrows
         ) ;
         out.names() = visitor_names ;
         return out.asSexp() ;
    }
    
    
    // TODO: deal with na.last
    IntegerVector DataFrameVisitors::order(bool decreasing) const {
        int n = data.nrows() ;
        IntegerVector res = seq(0, n-1) ;
        if( decreasing ){
            std::sort( res.begin(), res.end(), DataFrameVisitorsGreater(*this) ) ; 
        } else {
            std::sort( res.begin(), res.end(), DataFrameVisitorsLess(*this) ) ;
        }
        for( int i=0; i<n; i++) res[i]++ ;
        return res ;
    }
    
}
