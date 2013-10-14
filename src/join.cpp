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

#include <dplyr.h>

using namespace Rcpp ;
using namespace dplyr ;

CharacterVector common_by( CharacterVector x, CharacterVector y){
    return intersect(x, y) ;    
}

DataFrame subset( DataFrame df, const std::vector<int>& indices, CharacterVector columns){
    DataFrameVisitors visitors(df, columns) ;
    return visitors.copy(indices) ;
}

// [[Rcpp::export]]
DataFrame semi_join_impl( DataFrame x, DataFrame y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    CharacterVector by   = common_by(x.names(), y.names()) ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);  
    
    // train the map in terms of x
    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++)
        map[i].push_back(i) ;
    
    int n_y = y.nrows() ;
    std::vector<int> indices ;
    for( int i=0; i<n_y; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            std::vector<int>& chunk = it->second ;
            indices.insert( indices.end(), chunk.begin(), chunk.end() ) ;
            map.erase(it) ;
        }
    }
    
    return subset(x, indices, x.names() ) ;
}

// [[Rcpp::export]]
DataFrame anti_join_impl( DataFrame x, DataFrame y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    CharacterVector by   = common_by(x.names(), y.names()) ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);  
    
    // train the map in terms of x
    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++)
        map[i].push_back(i) ;
    
    int n_y = y.nrows() ;
    // remove the rows in x that match
    for( int i=0; i<n_y; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() )
            map.erase(it) ;
    }
    
    // collect what's left
    std::vector<int> indices ;
    Map::iterator it = map.begin() ;
    for( ; it != map.end(); ++it){
        std::vector<int>& chunk = it->second ;
        indices.insert( indices.end(), chunk.begin(), chunk.end() ) ;    
    }
    
    return subset(x, indices, x.names() ) ;
}

// [[Rcpp::export]]
DataFrame inner_join_impl( DataFrame x, DataFrame y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    CharacterVector by   = common_by(x.names(), y.names()) ;
    DataFrameJoinVisitors visitors(x, y, by) ;
    Map map(visitors);  
    
    // train the map in terms of x
    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++)
        map[i].push_back(i) ;
    
    std::vector<int> indices_x ;
    std::vector<int> indices_y ;
    
    int n_y = y.nrows() ;
    for( int i=0; i<n_y; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            std::vector<int>& chunk = it->second ;
            indices_x.insert( indices_x.end(), chunk.begin(), chunk.end() ) ;  
            
            int chunk_size = chunk.size() ;
            for( int j=0; j<chunk_size; j++)
                indices_y.push_back(i) ;
        }
    }

    CharacterVector x_columns = x.names() ;
    DataFrameVisitors visitors_x(x, x_columns) ;
    
    CharacterVector all_y_columns = y.names() ;
    CharacterVector y_columns = setdiff( all_y_columns, by ) ;
    DataFrameVisitors visitors_y(y, y_columns) ;
    
    int nrows = indices_x.size() ;
    int nv_x = visitors_x.size(), nv_y = visitors_y.size() ;
    List out(nv_x+nv_y);
    CharacterVector names(nv_x+nv_y) ;
    int k=0;
    for( ; k<nv_x; k++){
       out[k] = visitors_x.get(k)->copy(indices_x) ;
       names[k] = x_columns[k] ;
    }
    for( int i=0; i<nv_y; i++, k++){
       out[k] = visitors_y.get(i)->copy(indices_y) ; 
       names[k] = y_columns[i] ;
    }
    out.attr("class") = "data.frame" ;
    out.attr("row.names") = IntegerVector::create( 
        IntegerVector::get_na(), -nrows
    ) ;
    out.names() = names ;
    return out.asSexp() ;
}


// [[Rcpp::export]]
DataFrame left_join_impl( DataFrame x, DataFrame y){
    typedef VisitorSetIndexMap<DataFrameJoinVisitors, std::vector<int> > Map ;
    CharacterVector by   = common_by(x.names(), y.names()) ;
    DataFrameJoinVisitors visitors(y, x, by) ;
    Map map(visitors);  
    
    // train the map in terms of y
    int n_y = y.nrows() ;
    for( int i=0; i<n_y; i++)
        map[i].push_back(i) ;
    
    std::vector<int> indices_x ;
    std::vector<int> indices_y ;
    
    int n_x = x.nrows() ;
    for( int i=0; i<n_x; i++){
        Map::iterator it = map.find(-i-1) ;
        if( it != map.end() ){
            std::vector<int>& chunk = it->second ;
            indices_y.insert( indices_y.end(), chunk.begin(), chunk.end() ) ;  
            
            int chunk_size = chunk.size() ;
            for( int j=0; j<chunk_size; j++)
                indices_x.push_back(i) ;
        } else {
            indices_y.push_back(-1) ;
            indices_x.push_back(i) ;
        }
    }

    CharacterVector x_columns = x.names() ;
    DataFrameVisitors visitors_x(x, x_columns) ;
    
    CharacterVector all_y_columns = y.names() ;
    CharacterVector y_columns = setdiff( all_y_columns, by ) ;
    DataFrameVisitors visitors_y(y, y_columns) ;
    
    int nrows = indices_x.size() ;
    int nv_x = visitors_x.size(), nv_y = visitors_y.size() ;
    List out(nv_x+nv_y);
    CharacterVector names(nv_x+nv_y) ;
    int k=0;
    for( ; k<nv_x; k++){
       out[k] = visitors_x.get(k)->copy_perhaps_na(indices_x) ;
       names[k] = x_columns[k] ;
    }
    for( int i=0; i<nv_y; i++, k++){
       out[k] = visitors_y.get(i)->copy_perhaps_na(indices_y) ; 
       names[k] = y_columns[i] ;
    }
    out.attr("class") = "data.frame" ;
    out.attr("row.names") = IntegerVector::create( 
        IntegerVector::get_na(), -nrows
    ) ;
    out.names() = names ;
    return out.asSexp() ;
}

