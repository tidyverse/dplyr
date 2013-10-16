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

#ifndef dplyr_Result_H
#define dplyr_Result_H

namespace dplyr {
    
    // we can either derive from Result directly and implement process
    // manually by traversing the ChunkIndexMap
    // or we can use the Processor template class
    class Result {
    public:
        Result(){}
        virtual ~Result(){} ;
        
        virtual SEXP process( const Rcpp::GroupedDataFrame& gdf) = 0 ;
        
        virtual SEXP process( const Rcpp::FullDataFrame& df ) = 0 ;
    } ;

} // namespace dplyr

#endif
