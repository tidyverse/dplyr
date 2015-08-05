#ifndef dplyr_MatrixColumnSubsetVisitor_H
#define dplyr_MatrixColumnSubsetVisitor_H

namespace dplyr {

    template <int RTYPE>
    class MatrixColumnSubsetVisitor : public SubsetVectorVisitor {
    public:

        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef typename Matrix<RTYPE>::Column Column ;

        MatrixColumnSubsetVisitor( const Matrix<RTYPE>& data_ ) : data(data_){}

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
    } ;

}

#endif
