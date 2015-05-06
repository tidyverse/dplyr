#ifndef dplyr_DataFrameColumnVisitors_H
#define dplyr_DataFrameColumnVisitors_H

namespace dplyr {

    class DataFrameColumnVisitor : public VectorVisitor {
    public:
        DataFrameColumnVisitor( const DataFrame& data_ ) : data(data_), visitors(data) {}
        
        inline size_t hash(int i) const {
            return visitors.hash(i) ;    
        }
        
        inline bool equal(int i, int j) const {
            return visitors.equal(i,j) ;    
        }
        
        inline bool less( int i, int j ) const {
            return visitors.less(i,j) ;    
        }
        
        inline bool greater( int i, int j ) const {
            return visitors.greater(i,j) ;    
        }
        
        inline SEXP subset( const Rcpp::IntegerVector& index ) const {
            return visitors.subset( index, data.attr("class") ) ;    
        }
        
        inline SEXP subset( const std::vector<int>& index ) const {
            return visitors.subset( index, data.attr("class")  ) ;    
        }
        
        inline SEXP subset( const ChunkIndexMap& index ) const {
            return visitors.subset( index, data.attr("class")  ) ;    
        }
        
        inline SEXP subset( const Rcpp::LogicalVector& index ) const {
            return visitors.subset( index, data.attr("class") ) ;    
        }
        
        inline SEXP subset( EmptySubset index ) const {
            return visitors.subset( index, data.attr("class")  );
        }
        
        virtual int size() const {
            return visitors.nrows() ;    
        }
        
        virtual std::string get_r_type() const {
            return "data.frame"  ;    
        }
        
        virtual bool is_compatible( VectorVisitor* other, std::stringstream&, const std::string& ) const {
            return true ;
        }
        
    private:
        DataFrame data ;
        DataFrameVisitors visitors ;
    } ;
    
}

#endif
