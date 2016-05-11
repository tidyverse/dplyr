#ifndef dplyr_Result_VectorSliceVisitor_H
#define dplyr_Result_VectorSliceVisitor_H

namespace dplyr {

    template <int RTYPE>
    class VectorSliceVisitor {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        VectorSliceVisitor( SEXP data_, const SlicingIndex& index_ ) :
            data(data_),
            ptr( Rcpp::internal::r_vector_start<RTYPE>(data)),
            n(index_.size()),
            index(index_)
        {}

        inline STORAGE operator[]( int i) const {
            return ptr[index[i]];
        }

        inline int size() const {
            return n ;
        }

        inline operator SEXP() const {
            return wrap_subset<RTYPE>(data, index) ;
        }

    private:
        SEXP data ;
        STORAGE* ptr ;
        int n ;
        const SlicingIndex& index;
    } ;

}

#endif
