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
            copy_most_attributes(res, first_result ) ;
        }

        DelayedProcessor( int i, const RObject& chunk, SEXP res_ ) :
            res( as<Vec>( res_ ) )
        {
          copy_most_attributes( res, chunk ) ;
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
            copy_most_attributes(res, first_result) ;
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
    class FactorDelayedProcessor : public DelayedProcessor_Base<CLASS>{
    private:
        typedef dplyr_hash_map<SEXP,int> LevelsMap ;

    public:

        FactorDelayedProcessor(int first_non_na, SEXP first_result, int ngroups ) :
          res(ngroups, NA_INTEGER)
        {
          copy_most_attributes( res, first_result ) ;
          CharacterVector levels = Rf_getAttrib( first_result, Rf_install("levels") ) ;
          int n = levels.size() ;
          for( int i=0; i<n; i++) levels_map[ levels[i] ] = i+1 ;
        }

        virtual bool handled(int i, const RObject& chunk ) {
            CharacterVector lev = chunk.attr("levels") ;
            update_levels(lev) ;

            int val = as<int>(chunk) ;
            if( val == NA_INTEGER){
              return true ;
            }
            SEXP s = lev[val-1] ;
            res[i] = levels_map[s] ;
            return true ;
        }
        virtual bool can_promote(const RObject& chunk ) {
            return false ;
        }
        virtual DelayedProcessor_Base<CLASS>* promote(int i, const RObject& chunk) {
            return 0 ;
        }
        virtual SEXP get() {
            int n = levels_map.size() ;
            CharacterVector levels(n) ;
            LevelsMap::iterator it = levels_map.begin() ;
            for(int i=0; i<n; i++, ++it){
              levels[it->second-1] = it->first ;
            }
            res.attr("class") = "factor" ;
            res.attr("levels") = levels ;
            return res ;
        }

    private:

        void update_levels( const CharacterVector& lev) {
          int nlevels = levels_map.size() ;
          int n = lev.size() ;
          for(int i=0; i<n; i++) {
            SEXP s = lev[i] ;
            if( ! levels_map.count(s) ) {
              levels_map.insert( std::make_pair(s, ++nlevels) ) ;
            }
          }
        }

        IntegerVector res ;
        LevelsMap levels_map ;
    } ;



    template <typename CLASS>
    class DelayedProcessor<VECSXP, CLASS> : public DelayedProcessor_Base<CLASS> {
    public:
        DelayedProcessor(int first_non_na_, SEXP first_result, int ngroups) :
            res(ngroups)
        {
            res[first_non_na_] = maybe_copy(VECTOR_ELT(first_result, 0)) ;
            copy_most_attributes(res, first_result) ;
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
        if( Rf_inherits(first_result, "factor") ){
            return new FactorDelayedProcessor<CLASS>(i, first_result, ngroups) ;
        } else if( Rcpp::is<int>( first_result ) ){
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
