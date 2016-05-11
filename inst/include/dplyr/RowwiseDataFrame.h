#ifndef dplyr_tools_RowwiseDataFrame_H
#define dplyr_tools_RowwiseDataFrame_H

namespace Rcpp {

    class RowwiseDataFrame ;

    class RowwiseDataFrameIndexIterator {
    public:
        RowwiseDataFrameIndexIterator() : i(0){}

        RowwiseDataFrameIndexIterator& operator++() {
            ++i ;
            return *this ;
        }

        SlicingIndex operator*() const {
            return SlicingIndex( IntegerVector::create(i), i) ;
        }

        int i ;
    } ;

    class RowwiseDataFrame {
    public:
        typedef RowwiseDataFrameIndexIterator group_iterator ;
        RowwiseDataFrame( SEXP x):
            data_(x),
            group_sizes()
        {
            group_sizes = rep(1, data_.nrows()) ;
        }

        group_iterator group_begin() const {
            return RowwiseDataFrameIndexIterator() ;
        }

        DataFrame& data() {
            return data_ ;
        }
        const DataFrame& data() const {
            return data_ ;
        }

        inline int ngroups() const {
            return group_sizes.size() ;
        }

        inline int nvars() const {
            return 0 ;
        }

        inline SEXP symbol(int i){ return R_NilValue ; }
        inline SEXP label(int i){ return R_NilValue ; }

        inline int nrows() const {
            return data_.nrows() ;
        }

        inline int max_group_size() const{
            return 1 ;
        }

        inline const IntegerVector& get_group_sizes() const {
            return group_sizes ;
        }

    private:

        DataFrame data_ ;
        IntegerVector group_sizes ;

    } ;

    template <>
    inline bool is<RowwiseDataFrame>( SEXP x){
        return Rf_inherits(x, "rowwise_df") ;
    }

}

#endif
