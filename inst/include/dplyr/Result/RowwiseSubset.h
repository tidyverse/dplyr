#ifndef dplyr_RowwiseSubset_H
#define dplyr_RowwiseSubset_H

namespace dplyr {

    class RowwiseSubset {
    public:
        RowwiseSubset(){} ;
        virtual ~RowwiseSubset(){} ;
        virtual SEXP get( const SlicingIndex& indices ) = 0 ;
        virtual SEXP get_variable() const = 0 ;
        virtual bool is_summary() const = 0;
    } ;

    template <int RTYPE>
    class RowwiseSubsetTemplate : public RowwiseSubset {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        RowwiseSubsetTemplate( SEXP x ) :
            object(x), output(1), start( Rcpp::internal::r_vector_start<RTYPE>(object) )
        {
            copy_most_attributes( output, x) ;
        }

        virtual SEXP get( const SlicingIndex& indices ) {
            output[0] = start[ indices.group() ] ;
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
        Vector<RTYPE> output ;
        STORAGE* start ;
    } ;

    template <>
    class RowwiseSubsetTemplate<VECSXP> : public RowwiseSubset {
    public:
        RowwiseSubsetTemplate( SEXP x) :
            object(x), start( Rcpp::internal::r_vector_start<VECSXP>(object) )
        {}

        virtual SEXP get( const SlicingIndex& indices ) {
            return start[ indices.group() ] ;
        }
        virtual SEXP get_variable() const {
            return object ;
        }
        virtual bool is_summary() const {
            return false;
        }

    private:
        SEXP object ;
        SEXP* start ;
    } ;


    inline RowwiseSubset* rowwise_subset(SEXP x){
        switch( TYPEOF(x) ){
            case INTSXP: return new RowwiseSubsetTemplate<INTSXP>(x) ;
            case REALSXP: return new RowwiseSubsetTemplate<REALSXP>(x) ;
            case LGLSXP: return new RowwiseSubsetTemplate<LGLSXP>(x) ;
            case STRSXP: return new RowwiseSubsetTemplate<STRSXP>(x) ;
            case VECSXP: return new RowwiseSubsetTemplate<VECSXP>(x) ;
        }
        return 0 ;
    }

}

#endif
