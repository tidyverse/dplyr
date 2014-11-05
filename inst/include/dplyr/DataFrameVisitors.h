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
                                    
            DataFrameVisitors( const Rcpp::DataFrame& data_) :
                data(data_), 
                visitors()
            {
                visitor_names = get_DataFrame_names(data) ;
                nvisitors =  visitor_names.size() ;
                             
                int index = 0 ;
                fill_all_visitors(index, data) ;
                
            }
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
                    SEXP column = get_column(data, name) ; 
                    
                    if( column == R_NilValue ){
                        std::stringstream s ;
                        s << "unknown column '"
                          << name
                          << "'"; 
                        stop(s.str()); 
                    }
                    
                    nvisitors++ ;
                    visitor_names_.push_back( name ) ;
                    visitors.push_back( visitor( column ) ) ;
                    
                }
                visitor_names = wrap( visitor_names_ );
                
            } 
            
            ~DataFrameVisitors(){
                delete_all( visitors );
            }     
            
            
            template <typename Container>
            DataFrame subset( const Container& index, const CharacterVector& classes ) const {
                List out(nvisitors);
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
            
            inline SEXP get_column( const Rcpp::DataFrame& df, std::string& name ){
                int n = df.size() ;
                CharacterVector names = df.names() ;
                for(int i=0; i<n; i++){
                    SEXP column  = df[i] ;
                    if( Rf_inherits( column, "data.frame" ) ){
                        SEXP res = get_column( column, name ) ;
                        if( res != R_NilValue ) return res ;
                    } else {
                        String column_name = names[i] ;
                        if( column_name == name ) return column ;
                    }
                }
                return R_NilValue ;
            }
            
            inline void structure( List& x, int nrows, CharacterVector classes ) const {
                x.attr( "class" ) = classes ;
                set_rownames(x, nrows) ;
                x.names() = visitor_names ;
                SEXP vars = data.attr( "vars" ) ;
                if( !Rf_isNull(vars) )
                    x.attr( "vars" ) = vars ;
            }
            
            inline void fill_all_visitors( int& index, const Rcpp::DataFrame& df){
                int n = df.size() ;
                CharacterVector names = df.names() ;
                for(int i=0; i<n; i++){
                    SEXP column = df[i] ;
                    if( Rf_inherits( column, "data.frame" ) ){
                        fill_all_visitors( index, column ) ;        
                    } else {
                        visitors.push_back( visitor(column) ) ;
                        index++ ;
                    }
                }
            }
        
            inline int get_DataFrame_column_count( const Rcpp::DataFrame& df ){
                int nc = 0 ;
                
                int n = df.size() ;
                for( int i=0; i<n; i++){
                    SEXP column = df[i] ;
                    if( Rf_inherits( column, "data.frame" ) ){
                        nc += get_DataFrame_column_count( column ) ;    
                    } else {
                        nc++ ;    
                    }
                }
                return nc ;
            }
            
            inline void fill_names( CharacterVector& out, int& index, const Rcpp::DataFrame& df){
                int n = df.size() ;
                CharacterVector names = df.names() ;
                for(int i=0; i<n; i++){
                    SEXP column = df[i] ;
                    if( Rf_inherits( column, "data.frame" ) ){
                        fill_names( out, index, column ) ;
                    } else {
                        out[index] = names[i] ;
                        index++ ;
                    }
                }
                
            }
            
            inline CharacterVector get_DataFrame_names(const Rcpp::DataFrame& df ){
                // first count how many columns
                int nc = get_DataFrame_column_count(df) ;
                
                CharacterVector out(nc) ;
                int index = 0 ;
                fill_names(out, index, df ) ;
                return out ;
            }
    
            
    } ;
          
    inline DataFrame subset( DataFrame data, LogicalVector test, CharacterVector select, CharacterVector classes ){
        DataFrameVisitors visitors( data, select ) ;
        return visitors.subset(test, classes ) ;
    }

    inline DataFrame subset( DataFrame data, LogicalVector test, CharacterVector classes ){
        DataFrameVisitors visitors( data ) ;
        return visitors.subset(test, classes ) ;
    }


} // namespace dplyr


#endif
