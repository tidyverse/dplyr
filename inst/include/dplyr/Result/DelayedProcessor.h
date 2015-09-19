#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

namespace dplyr{

    template <typename CLASS>
    class DelayedProcessor_Base {
       public:
           DelayedProcessor_Base(){}
           virtual ~DelayedProcessor_Base(){}

           virtual bool handled(int i, const RObject& chunk ) = 0 ;
           virtual bool can_promote(const RObject& chunk ) = 0 ;
           virtual DelayedProcessor_Base* promote(int i, const RObject& chunk) = 0 ;
           virtual SEXP get() = 0;
    } ;

    template <int RTYPE>
    bool valid_conversion(int rtype){
        return rtype == RTYPE ;
    }

    template <>
    inline bool valid_conversion<REALSXP>( int rtype ){
        switch( rtype ){
        case REALSXP:
        case INTSXP:
        case LGLSXP:
            return true ;
        default: break ;
        }
        return false ;
    }

    template <>
    inline bool valid_conversion<INTSXP>( int rtype ){
        switch( rtype ){
        case INTSXP:
        case LGLSXP:
            return true ;
        default: break ;
        }
        return false ;
    }

    template <int RTYPE>
    inline bool valid_promotion(int rtype) {
        return false ;
    }

    template <>
    inline bool valid_promotion<INTSXP>( int rtype ){
        return rtype == REALSXP ;
    }

    template <>
    inline bool valid_promotion<LGLSXP>( int rtype ){
        return rtype == REALSXP || rtype == INTSXP ;
    }

    template <int RTYPE, typename CLASS>
    class DelayedProcessor : public DelayedProcessor_Base<CLASS> {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef Vector<RTYPE> Vec ;

        DelayedProcessor( int first_non_na, SEXP first_result, int ngroups_) :
            res( no_init(ngroups_) )
        {
            std::fill( res.begin(), res.begin() + first_non_na, Vec::get_na() );
            res[first_non_na] = as<STORAGE>( first_result ) ;
        }

        DelayedProcessor( int i, const RObject& chunk, SEXP res_ ) :
            res( as<Vec>( res_ ) )
        {
            res[i] = as<STORAGE>(chunk) ;
        }

        virtual bool handled(int i, const RObject& chunk ) {
            int rtype = TYPEOF(chunk) ;
            if( valid_conversion<RTYPE>(rtype) ){
                res[i] = as<STORAGE>( chunk ) ;
                return true ;
            } else {
                return false ;
            }
        }

        virtual bool can_promote(const RObject& chunk ) {
            return valid_promotion<RTYPE>( TYPEOF(chunk) ) ;
        }
        virtual DelayedProcessor_Base<CLASS>* promote(int i, const RObject& chunk){
            int rtype = TYPEOF(chunk) ;
            switch( rtype ){
            case LGLSXP:  return new DelayedProcessor<LGLSXP , CLASS>(i, chunk, res ) ;
            case INTSXP:  return new DelayedProcessor<INTSXP , CLASS>(i, chunk, res ) ;
            case REALSXP: return new DelayedProcessor<REALSXP, CLASS>(i, chunk, res ) ;
            case CPLXSXP: return new DelayedProcessor<CPLXSXP, CLASS>(i, chunk, res ) ;
            default: break ;
            }
            return 0 ;
        }

        virtual SEXP get() {
            return res ;
        }


    private:
        Vec res ;


    } ;

    template <typename CLASS>
    class DelayedProcessor<STRSXP, CLASS> : public DelayedProcessor_Base<CLASS> {
    public:
        DelayedProcessor(int first_non_na_, SEXP first_result, int ngroups) :
            res(ngroups)
        {
            res[first_non_na_] = as<String>(first_result) ;
        }

        virtual bool handled(int i, const RObject& chunk ) {
            res[i] = as<String>(chunk) ;
            return true ;
        }
        virtual bool can_promote(const RObject& chunk ) {
            return false ;
        }
        virtual DelayedProcessor_Base<CLASS>* promote(int i, const RObject& chunk) {
            return 0 ;
        }
        virtual SEXP get() {
            return res ;
        }

    private:
        CharacterVector res ;
    } ;

    template <typename CLASS>
    class DelayedProcessor<VECSXP, CLASS> : public DelayedProcessor_Base<CLASS> {
    public:
        DelayedProcessor(int first_non_na_, SEXP first_result, int ngroups) :
            res(ngroups)
        {
            res[first_non_na_] = maybe_copy(VECTOR_ELT(first_result, 0)) ;
        }

        virtual bool handled(int i, const RObject& chunk ) {
            if( is<List>(chunk) && Rf_length(chunk) == 1){
                res[i] = maybe_copy(VECTOR_ELT(chunk, 0)) ;
                return true ;
            }
            return false ;
        }
        virtual bool can_promote(const RObject& chunk ) {
            return false ;
        }
        virtual DelayedProcessor_Base<CLASS>* promote(int i, const RObject& chunk) {
            return 0 ;
        }
        virtual SEXP get() {
            return res ;
        }

    private:
        List res ;

        inline SEXP maybe_copy(SEXP x) const {
            return is_ShrinkableVector(x) ? Rf_duplicate(x) : x ;
        }


    } ;

    template <typename CLASS>
    DelayedProcessor_Base<CLASS>* get_delayed_processor(int i, SEXP first_result, int ngroups){
        if( Rcpp::is<int>( first_result ) ){
            return new DelayedProcessor<INTSXP, CLASS>(i, first_result, ngroups) ;
        } else if( Rcpp::is<double>( first_result) ){
            return new DelayedProcessor<REALSXP, CLASS>(i, first_result, ngroups) ;
        } else if( Rcpp::is<Rcpp::String>( first_result) ){
            return new DelayedProcessor<STRSXP, CLASS>(i, first_result, ngroups) ;
        } else if( Rcpp::is<bool>( first_result) ){
            return new DelayedProcessor<LGLSXP, CLASS>(i, first_result, ngroups) ;
        } else if( Rcpp::is<Rcpp::List>( first_result ) ){
            if( Rf_length(first_result) != 1 ) return 0 ;
            return new DelayedProcessor<VECSXP, CLASS>(i, first_result, ngroups) ;
        } else if( Rf_length(first_result) == 1 && TYPEOF(first_result) == CPLXSXP ){
            return new DelayedProcessor<CPLXSXP, CLASS>(i, first_result, ngroups) ;
        }
        return 0 ;
    }

}
#endif
