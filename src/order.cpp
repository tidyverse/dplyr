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
using namespace dplyr ;

// [[Rcpp::export]] 
IntegerVector order_impl( List args, Environment env ){
    int nargs = args.size() ;  
    SEXP tmp ;
    List variables(nargs) ; 
    LogicalVector ascending(nargs) ;
    for(int i=0; i<nargs; i++){
        tmp = args[i] ;
        if( TYPEOF(tmp) == LANGSXP && CAR(tmp) == Rf_install("desc") ){
            variables[i] = Rf_eval( CAR(CDR(tmp) ), env ) ;
            ascending[i] = false ;
        } else{
            variables[i] = Rf_eval( tmp, env );
            ascending[i] = true ;
        }
    }
    OrderVisitors o(variables,ascending, nargs) ;
	IntegerVector res = o.apply() ;
	res = res + 1 ;
	return res ;
}

// [[Rcpp::export]] 
DataFrame arrange_impl( DataFrame data, List args, Environment env ){
    int nargs = args.size() ;  
    SEXP tmp ;
    List variables(nargs) ; 
    LogicalVector ascending(nargs) ;
    for(int i=0; i<nargs; i++){
        tmp = args[i] ;
        if( TYPEOF(tmp) == LANGSXP && CAR(tmp) == Rf_install("desc") ){
            variables[i] = Rf_eval( CAR(CDR(tmp) ), env ) ;
            ascending[i] = false ;
        } else{
            variables[i] = Rf_eval( tmp, env );
            ascending[i] = true ;
        }
    }
    OrderVisitors o(variables,ascending, nargs) ;
	IntegerVector index = o.apply() ;
	
	DataFrameVisitors visitors( data, data.names() ) ;
	DataFrame res = visitors.copy(index) ;
	return res;
}

// [[Rcpp::export]] 
DataFrame sort_impl( DataFrame data ){
    OrderVisitors o(data) ;
    IntegerVector index = o.apply() ;
    
    DataFrameVisitors visitors( data, data.names() ) ;
    DataFrame res = visitors.copy(index) ;
    return res;
}

namespace dplyr {
    
    OrderVisitors::OrderVisitors( List args, Rcpp::LogicalVector ascending, int n_ ) : 
        visitors(n_), n(n_), nrows(0){
        nrows = Rf_length( args[0] );
        for( int i=0; i<n; i++)
            visitors[i]  = order_visitor( args[i], ascending[i] );
    } 
    
    OrderVisitors::OrderVisitors( DataFrame data ) : 
        visitors(data.size()), n(data.size()), nrows( data.nrows() )
    {
        for( int i=0; i<n; i++)
            visitors[i]  = order_visitor( data[i], true );
    } 
    
    OrderVisitors::OrderVisitors( DataFrame data, CharacterVector names ) : 
        visitors(data.size()), n(data.size()), nrows( data.nrows() )
    {
        for( int i=0; i<n; i++){
            String name = names[i] ;
            visitors[i]  = order_visitor( data[name], true );
        }
    } 
    
    OrderVisitors_Compare::OrderVisitors_Compare( const OrderVisitors& obj_ ) : 
        obj(obj_), n(obj.n){}
    
    IntegerVector OrderVisitors::apply() const {
        IntegerVector x = seq(0, nrows -1 ) ;
        std::sort( x.begin(), x.end(), OrderVisitors_Compare(*this) ) ;
        return x ;
    }
    
    bool OrderVisitors_Compare::operator()(int i, int j) const {
        if( i == j ) return false ;
        for( int k=0; k<n; k++)
            if( ! obj.visitors[k]->equal(i,j) )
                return obj.visitors[k]->before(i, j ) ; 
        return i < j ;
        
    }

    OrderVisitors::~OrderVisitors(){
        delete_all( visitors ) ;
    }
    
    
}
