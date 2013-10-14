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

#ifndef dplyr_Order_H
#define dplyr_Order_H

namespace dplyr {
      
class OrderVisitors ;  
class OrderVisitors_Compare {
public:
    OrderVisitors_Compare( const OrderVisitors& obj_ ) ; 
    
    bool operator()(int i, int j) const ;
    
private:
    const OrderVisitors& obj ;
    int n ;
    
} ;
    
class OrderVisitors {
public:
    
    OrderVisitors( Rcpp::List args, Rcpp::LogicalVector ascending_, int n_ ) ;
    OrderVisitors( Rcpp::DataFrame data ) ;
    OrderVisitors( Rcpp::DataFrame data, Rcpp::CharacterVector names ) ;
    ~OrderVisitors() ;
    
    Rcpp::IntegerVector apply() const  ;
    
    std::vector<OrderVisitor*> visitors ;
    int n;
    int nrows ;    
} ;    
    
} // namespace dplyr


#endif
