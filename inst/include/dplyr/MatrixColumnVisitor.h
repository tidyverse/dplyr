#ifndef dplyr_MatrixColumnVisitor_H
#define dplyr_MatrixColumnVisitor_H

namespace dplyr {
            
    template <int RTYPE>
    class MatrixColumnVisitor : public VectorVisitor {
    public:         
        
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef typename Matrix<RTYPE>::Column Column ;
        
        class ColumnVisitor : public comparisons<RTYPE> {
        public:
            typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
            typedef comparisons<RTYPE> compare ;
            typedef boost::hash<STORAGE> hasher ;
            
            ColumnVisitor( Matrix<RTYPE>& data, int column ) : 
                column( data.column(column) ) {}
            
            inline size_t hash(int i) const {
                return hash_fun( const_cast<Column&>(column)[i] ) ;
            }
            
            inline bool equal( int i, int j ) const {
                return compare::equal_or_both_na(  const_cast<Column&>(column)[i],  const_cast<Column&>(column)[j] ) ;    
            }
               
            inline bool less(int i, int j) const { 
                return compare::is_less(  const_cast<Column&>(column)[i],  const_cast<Column&>(column)[j] ) ;
            }
            
            inline bool equal_or_both_na(int i, int j) const {
                return compare::equal_or_both_na(  const_cast<Column&>(column)[i],  const_cast<Column&>(column)[j] ) ;    
            }
            
            inline bool greater(int i, int j) const { 
                return compare::is_greater(  const_cast<Column&>(column)[i],  const_cast<Column&>(column)[j] ) ;
            }
        
        private:
            Column column ;
            hasher hash_fun ;
        } ;
        
        MatrixColumnVisitor( const Matrix<RTYPE>& data_ ) : data(data_), visitors() {
            for( int h=0; h<data.ncol(); h++){
                visitors.push_back( ColumnVisitor( data, h ) ) ;    
            }
        }
        
        inline size_t hash(int i) const {
            size_t seed = visitors[0].hash(i) ;
            for( size_t h=1; h<visitors.size(); h++){
                boost::hash_combine( seed, visitors[h].hash(i) ) ;
            }
            return seed ;
        }
        
        inline bool equal(int i, int j) const {
            if( i == j ) return true ;
            for( size_t h=0; h<visitors.size(); h++){
                if( !visitors[h].equal(i,j) ) return false ;    
            }
            return true ;
        }
        
        inline bool less( int i, int j ) const {
            if( i == j ) return false ;
            for( size_t h=0; h<visitors.size(); h++){
                const ColumnVisitor& v = visitors[h] ;
                if( !v.equal(i,j) ){
                    return v.less(i,j) ;     
                }
            }
            return i < j ;
        }
        
        inline bool greater( int i, int j ) const {
            if( i == j ) return false ;
            for( size_t h=0; h<visitors.size(); h++){
                const ColumnVisitor& v = visitors[h] ;
                if( !v.equal(i,j) ){
                    return v.greater(i,j) ;     
                }
            }
            return i < j ;    
        }
        
        inline SEXP subset( const Rcpp::IntegerVector& index ) const {
            return subset_int( index ) ;
        }
        
        inline SEXP subset( const std::vector<int>& index ) const {
            return subset_int( index ) ;    
        }
        
        inline SEXP subset( const ChunkIndexMap& index ) const {
            int n = index.size() ;
            Matrix<RTYPE> res( n, data.ncol() ) ;
            for( size_t h=0; h<visitors.size(); h++){ 
                ChunkIndexMap::const_iterator it = index.begin(); 
                Column column = res.column(h) ;
                Column source_column = const_cast<Matrix<RTYPE>&>(data).column(h) ;
                
                for( int i=0; i<n; i++, ++it){
                    column[i] = source_column[ it->first ] ;
                }
            }
            return res ;    
        }
        
        inline SEXP subset( const Rcpp::LogicalVector& index ) const {
            int n = output_size(index) ;
            Matrix<RTYPE> res(n, data.ncol()) ;
            for( size_t h=0; h<visitors.size(); h++){
                Column column = res.column(h) ;
                Column source_column = const_cast<Matrix<RTYPE>&>(data).column(h) ;
                
                for( int i=0, k=0; k<n; k++, i++ ) {
                    while( index[i] != TRUE ) i++; 
                    column[k] = source_column[i] ;
                }
            }
            return res ;
        }
        
        inline SEXP subset( EmptySubset index ) const {
            return Matrix<RTYPE>( 0, data.ncol() );
        }
        
        inline int size() const {
            return data.nrow() ;    
        }
        
        inline std::string get_r_type() const {
            return "matrix"  ;    
        }
        
        inline bool is_compatible( VectorVisitor* other, std::stringstream&, const std::string& ) const {
            return true ;
        }
        
    private:
        
        template <typename Container>
        inline SEXP subset_int( const Container& index ) const {
            int n = index.size() ;
            Matrix<RTYPE> res( n, data.ncol() ) ;
            for( size_t h=0; h<visitors.size() ; h++){
                Column column = res.column(h) ;
                Column source_column = const_cast<Matrix<RTYPE>&>(data).column(h) ;
                for(int k=0; k< n; k++){
                    column[k] = source_column[ index[k] ] ;
                }
            }
            return res ;
        }
        
        Matrix<RTYPE> data ;
        std::vector<ColumnVisitor> visitors ;
    } ;
    
}

#endif
