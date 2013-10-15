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

#ifndef dplyr_VectorVisitor_H
#define dplyr_VectorVisitor_H

namespace dplyr {
    
/** 
 * Vector visitor base class, defines the interface
 */
class VectorVisitor {
public:
    virtual ~VectorVisitor(){}
    
    /** hash the element of the visited vector at index i */
    virtual size_t hash(int i) = 0 ;
    
    /** are the elements at indices i and j equal */
    virtual bool equal(int i, int j) = 0 ;
    
    /** is the i element less than the j element */
    virtual bool less( int i, int j) = 0 ;
    
    /** is the i element less than the j element */
    virtual bool greater( int i, int j) = 0 ;
    
    /** creates a new vector, of the same type as the visited vector, by 
     *  copying elements at the given indices
     */
    virtual SEXP copy( const Rcpp::IntegerVector& index ) = 0 ;
    
    virtual SEXP copy( const std::vector<int>& ) = 0 ;
    
    /** creates a new vector, of the same type as the visited vector, by 
     *  copying elements at the given indices
     */
    virtual SEXP copy( const ChunkIndexMap& index ) = 0 ;
    
    virtual SEXP subset( const Rcpp::LogicalVector& index, int n ) = 0 ;
    
} ;

/**
 * dispatch the SEXP, will call the appropriate instantiation of make_visitor
 */
VectorVisitor* visitor( SEXP vec ) ;

} // namespace dplyr


#endif
