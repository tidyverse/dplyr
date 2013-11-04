#ifndef dplyr_Collecter_H
#define dplyr_Collecter_H

namespace dplyr {
    
    class Collecter {
    public:
        virtual ~Collecter(){} ;
        virtual void collect( const SlicingIndex& index, SEXP v ) = 0 ;
        virtual SEXP get() = 0 ;
        virtual bool compatible(SEXP) const = 0 ;
        virtual bool can_promote(SEXP) const = 0 ;
    } ;
    
    template <int RTYPE>
    class Collecter_Impl : public Collecter {
    public:
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        
        Collecter_Impl( int n_ ): data( n_, Rcpp::traits::get_na<RTYPE>() ){}
        
        void collect( const SlicingIndex& index, SEXP v ){
            Vector<RTYPE> source(v) ;
            STORAGE* source_ptr = Rcpp::internal::r_vector_start<RTYPE>(source) ;
            for( int i=0; i<index.size(); i++){
                data[index[i]] = source_ptr[i] ;
            }
        }
        
        inline SEXP get(){
            return data ;    
        }
        
        inline bool compatible(SEXP x) const{
            return RTYPE == TYPEOF(x) ;    
        }
        
        bool can_promote(SEXP x) const {
            return false ;    
        }
        
    private:
        Vector<RTYPE> data ;
    } ;
    
    class FactorCollecter : public Collecter {
    public:
        typedef boost::unordered_map<SEXP,int> LevelsMap ;
        
        FactorCollecter( int n ): 
            data(n, IntegerVector::get_na() ), levels_map(), current_level(1) {}
        
        void collect( const SlicingIndex& index, SEXP v ){
            IntegerVector source(v) ;
            CharacterVector levels = source.attr( "levels" ) ;
            SEXP* levels_ptr = Rcpp::internal::r_vector_start<STRSXP>( levels) ;
            int* source_ptr = Rcpp::internal::r_vector_start<INTSXP>(source) ;
            for( int i=0; i<index.size(); i++){
                SEXP x = levels_ptr[ source_ptr[i] - 1 ] ;
                LevelsMap::const_iterator it = levels_map.find( x ) ;
                if( it == levels_map.end() ){
                    data[index[i]] = current_level ;
                    levels_map[x] = current_level++ ;   
                } else {
                    data[index[i]] = it->second ;    
                }
            }
        }
        
        inline SEXP get() {
            int nlevels = levels_map.size() ;
            CharacterVector levels(nlevels);
            LevelsMap::iterator it = levels_map.begin() ;
            for( int i=0; i<nlevels; i++, ++it){
                levels[it->second - 1] = it->first ;
            }
            data.attr( "levels" ) = levels ;
            data.attr( "class" ) = "factor" ;
            return data ;
        }
        
        inline bool compatible(SEXP x) const{
            return Rf_inherits( x, "factor" ) ;    
        }
        
        bool can_promote(SEXP x) const {
            return TYPEOF(x) == STRSXP ;    
        }
        
    private:
        IntegerVector data ;
        LevelsMap levels_map ;
        int current_level ;
    } ;
    
    template <>
    inline bool Collecter_Impl<INTSXP>::can_promote( SEXP x) const {
        return TYPEOF(x) == REALSXP ;
    }
    
    template <>
    inline bool Collecter_Impl<LGLSXP>::can_promote( SEXP x) const {
        return TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP ;
    }
    
    inline Collecter* collecter(SEXP model, int n){
        switch( TYPEOF(model) ){
        case INTSXP: 
            if( Rf_inherits(model, "factor") )
                return new FactorCollecter(n) ;
            return new Collecter_Impl<INTSXP>(n) ;
        case REALSXP: return new Collecter_Impl<REALSXP>(n) ;
        case LGLSXP: return new Collecter_Impl<LGLSXP>(n) ;
        case STRSXP: return new Collecter_Impl<STRSXP>(n) ;
        default: break ;
        }
        return 0 ;
    }
    
}

#endif
