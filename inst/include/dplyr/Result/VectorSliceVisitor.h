#ifndef dplyr_Result_VectorSliceVisitor_H
#define dplyr_Result_VectorSliceVisitor_H

namespace dplyr {
    
    template <int RTYPE>
    class VectorSliceVisitor {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ; 
        
        VectorSliceVisitor( SEXP data, const SlicingIndex& index ) 
            : ptr( Rcpp::internal::r_vector_start<RTYPE>(data) + index[0] ), 
              n(index.size()){}
        
        inline STORAGE operator[]( int i) const { 
            return ptr[i]; 
        }
        
        inline int size() const { 
            return n ; 
        }
        
        inline operator SEXP() const {
            return Vector<RTYPE>( ptr, ptr+n ) ;    
        }
          
    private:
        STORAGE* ptr ;
        int n ;
    } ;

}

#endif
