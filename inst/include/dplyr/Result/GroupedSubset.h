#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

namespace dplyr {
    
    class GroupedSubset {
    public:
        GroupedSubset(){} ;
        virtual ~GroupedSubset(){} ;
        virtual SEXP get( const SlicingIndex& indices ) = 0 ;
    } ;
    
    template <int RTYPE>
    class GroupedSubsetTemplate : public GroupedSubset {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        GroupedSubsetTemplate( SEXP x, int max_size ) : 
            object(x), output(max_size), start( Rcpp::internal::r_vector_start<RTYPE>(object)) {}
        
        virtual SEXP get( const SlicingIndex& indices ) {
            output.borrow( start + indices[0], indices.size() ) ;
            return output ;
        }
    private:
        SEXP object ;
        ShrinkableVector<RTYPE> output ;
        STORAGE* start ;
    } ;
    
    inline GroupedSubset* grouped_subset(SEXP x, int max_size){
        switch( TYPEOF(x) ){
            case INTSXP: return new GroupedSubsetTemplate<INTSXP>(x, max_size) ;
            case REALSXP: return new GroupedSubsetTemplate<REALSXP>(x, max_size) ;
            case STRSXP: return new GroupedSubsetTemplate<STRSXP>(x, max_size) ;
        }
        return 0 ;
    }
    
}

#endif
