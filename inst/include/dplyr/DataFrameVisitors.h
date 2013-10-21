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

#ifndef dplyr_DataFrameVisitors_H
#define dplyr_DataFrameVisitors_H

namespace dplyr {

class DataFrameVisitors : 
    public VisitorSetEqual<DataFrameVisitors>, 
    public VisitorSetHash<DataFrameVisitors>, 
    public VisitorSetLess<DataFrameVisitors>, 
    public VisitorSetGreater<DataFrameVisitors> {
    
    private:
        
        const Rcpp::DataFrame& data ;
        std::vector<VectorVisitor*> visitors ;
        Rcpp::CharacterVector visitor_names ;
        int nvisitors ;
        
    public:
        typedef VectorVisitor visitor_type ;
        
        // visit no column, will add columns with the add_visitor method
        DataFrameVisitors( const Rcpp::DataFrame& data_ ) ;
        
        // visiting only named columns
        DataFrameVisitors( const Rcpp::DataFrame& data_, const Rcpp::CharacterVector& names ) ;
        
        DataFrameVisitors& add_visitor( Rcpp::String column ) ;
        
        ~DataFrameVisitors() ; 
    
        template <typename Container>
        Rcpp::DataFrame subset( const Container& index) const {
            int nrows = index.size() ;
            Rcpp::List out(nvisitors);
            for( int k=0; k<nvisitors; k++){
               out[k] = visitors[k]->subset(index) ;    
            }
            out.attr("class") = "data.frame" ;
            set_rownames(out, nrows) ;
            out.names() = visitor_names ;
            return out.asSexp() ;
        }
        
        Rcpp::DataFrame subset( const Rcpp::LogicalVector& ) const ;
        
        inline int size() const { return nvisitors ; }
        inline VectorVisitor* get(int k) const { return visitors[k] ; }
        
        Rcpp::String name(int k) const { return visitor_names[k] ; }
        
        inline int nrows() const { return data.nrows() ; } 
} ;

} // namespace dplyr


#endif
