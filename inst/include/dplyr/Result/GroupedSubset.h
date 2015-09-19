#ifndef dplyr_GroupedSubset_H
#define dplyr_GroupedSubset_H

namespace dplyr {
    
    class GroupedSubset {
    public:
        GroupedSubset(){} ;
        virtual ~GroupedSubset(){} ;
        virtual SEXP get( const SlicingIndex& indices ) = 0 ;
        virtual SEXP get_variable() const = 0 ;
        virtual bool is_summary() const = 0; 
    } ;
    
    template <int RTYPE>
    class GroupedSubsetTemplate : public GroupedSubset {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        GroupedSubsetTemplate( SEXP x, int max_size ) : 
            object(x), output(max_size, object), start( Rcpp::internal::r_vector_start<RTYPE>(object) ) {}
        
        virtual SEXP get( const SlicingIndex& indices ) {
            output.borrow( indices, start ) ;
            return output ;
        }
        virtual SEXP get_variable() const {
            return object ;    
        }
        virtual bool is_summary() const {
            return false;    
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
            case LGLSXP: return new GroupedSubsetTemplate<LGLSXP>(x, max_size) ;
            case STRSXP: return new GroupedSubsetTemplate<STRSXP>(x, max_size) ;
            case VECSXP: return new GroupedSubsetTemplate<VECSXP>(x, max_size) ;
            case CPLXSXP: return new GroupedSubsetTemplate<CPLXSXP>(x, max_size) ;
        }
        return 0 ;
    }
    
    
    template <int RTYPE>
    class SummarisedSubsetTemplate : public GroupedSubset {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        SummarisedSubsetTemplate( SummarisedVariable x, int max_size ) : 
            object(x), output(1), i(0) {}
        
        virtual SEXP get( const SlicingIndex& indices ) {
            output[0] = object[indices.group()] ;
            return output ;
        }
        virtual SEXP get_variable() const {
            return object ;    
        }
        virtual bool is_summary() const {
            return true;    
        }
        
    private:
        Rcpp::Vector<RTYPE> object ;
        Rcpp::Vector<RTYPE> output ;
        int i ;
    } ;
    
    inline GroupedSubset* summarised_grouped_subset(SummarisedVariable x, int max_size){
        switch( TYPEOF(x) ){
            case LGLSXP: return new SummarisedSubsetTemplate<LGLSXP>(x, max_size) ;
            case INTSXP: return new SummarisedSubsetTemplate<INTSXP>(x, max_size) ;
            case REALSXP: return new SummarisedSubsetTemplate<REALSXP>(x, max_size) ;
            case STRSXP: return new SummarisedSubsetTemplate<STRSXP>(x, max_size) ;
        }
        return 0 ;
    }
}

#endif
