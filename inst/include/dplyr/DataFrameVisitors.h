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
            
            DataFrameVisitors( const Rcpp::DataFrame& data_, const Rcpp::CharacterVector& names ) : 
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
            
            ~DataFrameVisitors(){
                delete_all( visitors );
            }
        
            template <typename Container>
            Rcpp::DataFrame subset( const Container& index, const CharacterVector& classes ) const {
                Rcpp::List out(nvisitors);
                for( int k=0; k<nvisitors; k++){
                   out[k] = get(k)->subset(index) ;    
                }
                structure( out, Rf_length(out[0]) , classes) ;
                return (SEXP)out ;
            }
            
            inline int size() const { return nvisitors ; }
            inline VectorVisitor* get(int k) const { return visitors[k] ; }
            
            Rcpp::String name(int k) const { return visitor_names[k] ; }
            
            inline int nrows() const { return data.nrows() ; } 
            
        private:
            
            void structure( List& x, int nrows, CharacterVector classes ) const {
                x.attr( "class" ) = classes ;
                set_rownames(x, nrows) ;
                x.names() = visitor_names ;
                SEXP vars = data.attr( "vars" ) ;
                if( !Rf_isNull(vars) )
                    x.attr( "vars" ) = vars ;
            }
            
    } ;
          
    inline DataFrame subset( DataFrame data, LogicalVector test, CharacterVector select, CharacterVector classes ){
        DataFrameVisitors visitors( data, select ) ;
        return visitors.subset(test, classes ) ;
    }


} // namespace dplyr


#endif
