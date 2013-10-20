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

// [[Rcpp::export]]
DataFrame build_index_cpp( DataFrame data, ListOf<Symbol> symbols ){
    int nv=symbols.size() ;
    CharacterVector vars( symbols.size() ) ;
    for( int i=0; i<nv; i++)
        vars[i] = symbols[i].c_str() ;
    
    DataFrameVisitors visitors(data, vars) ;
    ChunkIndexMap map( visitors ) ;
    int n=data.nrows() ;
    for( int i=0; i<n; i++) 
        map[i].push_back(i+1) ; // FIXME: rm the +1 when using 0-based index
    
    DataFrame labels = visitors.subset(map) ;
    List index       = get_all_second<List,ChunkIndexMap>(map) ; 
    data.attr( "index" ) = index ;
    data.attr( "labels" ) = labels ;
    return data ;
}
