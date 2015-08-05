#ifndef dplyr_DataFrameColumnSubsetVisitors_H
#define dplyr_DataFrameColumnSubsetVisitors_H

namespace dplyr {

    class DataFrameColumnSubsetVisitor : public SubsetVectorVisitor {
    public:
        DataFrameColumnVisitor( const DataFrame& data_ ) : data(data_), visitors(data) {}

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

    private:
        DataFrame data ;
        DataFrameSubsetVisitors visitors ;
    } ;

}

#endif
