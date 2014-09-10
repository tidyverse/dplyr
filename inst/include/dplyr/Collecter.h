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
        virtual bool is_factor_collecter() const{ 
            return false ;    
        }
        virtual std::string describe() const = 0 ;
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
        
        std::string describe() const {
            return vector_class<RTYPE>() ; 
        }
        
    protected:
        Vector<RTYPE> data ;
    } ;
    
    template <>
    class Collecter_Impl<REALSXP> : public Collecter {
    public:
        Collecter_Impl( int n_ ): data( n_, NA_REAL ){}
        
        void collect( const SlicingIndex& index, SEXP v ){
            NumericVector source(v) ;
            double* source_ptr = source.begin() ;
            for( int i=0; i<index.size(); i++){
                data[index[i]] = source_ptr[i] ;
            }
        }
        
        inline SEXP get(){
            return data ;    
        }
        
        inline bool compatible(SEXP x) const{
            int RTYPE = TYPEOF(x) ;
            return RTYPE == REALSXP || RTYPE == INTSXP || RTYPE == LGLSXP ;    
        }
        
        bool can_promote(SEXP x) const {
            return false ;    
        }
        
        std::string describe() const {
            return "numeric" ; 
        }
        
    protected:
        NumericVector data ;
        
    } ;
    
    template <>
    class Collecter_Impl<STRSXP> : public Collecter {
    public:
        Collecter_Impl( int n_ ): data( n_ ){}
        
        void collect( const SlicingIndex& index, SEXP v ){
            if( TYPEOF(v) == STRSXP ){
                collect_strings(index, v) ;
            } else if( Rf_inherits( v, "factor" ) ){
                collect_factor(index, v) ;
            }
        }
        
        inline SEXP get(){
            return data ;    
        }
        
        inline bool compatible(SEXP x) const{
            return ( STRSXP == TYPEOF(x) ) || Rf_inherits( x, "factor" ) ;
        }
        
        bool can_promote(SEXP x) const {
            return false ;    
        }
        
        std::string describe() const {
            return "integer" ; 
        }
        
    protected:
        CharacterVector data ;
        
    private:
        
        void collect_strings( const SlicingIndex& index, CharacterVector source){
            for( int i=0; i<index.size(); i++){
                data[index[i]] = source[i] ;
            }
        }
        
        void collect_factor( const SlicingIndex& index, IntegerVector source ){
            CharacterVector levels = source.attr("levels") ;
            for( int i=0; i<index.size(); i++){
                if( source[i] == NA_INTEGER ) {
                    data[index[i]] = NA_STRING ;    
                } else{
                    data[index[i]] = levels[source[i]-1] ;
                }
            }
        }
        
    } ;
    
    template <>
    class Collecter_Impl<INTSXP> : public Collecter {
    public:
        Collecter_Impl( int n_ ): data( n_, NA_INTEGER ){}
        
        void collect( const SlicingIndex& index, SEXP v ){
            IntegerVector source(v) ;
            int* source_ptr = source.begin() ;
            for( int i=0; i<index.size(); i++){
                data[index[i]] = source_ptr[i] ;
            }
        }
        
        inline SEXP get(){
            return data ;    
        }
        
        inline bool compatible(SEXP x) const{
            int RTYPE = TYPEOF(x) ;
            return ( INTSXP == RTYPE || RTYPE == LGLSXP ) && !Rf_inherits( x, "factor" ) ;
        }
        
        bool can_promote(SEXP x) const {
            return TYPEOF(x) == REALSXP || Rf_inherits( x, "factor" ) ;    
        }
        
        std::string describe() const {
            return "integer" ; 
        }
        
    protected:
        IntegerVector data ;
        
    } ;
    
    template <int RTYPE>
    class TypedCollecter : public Collecter_Impl<RTYPE>{
    public:    
        TypedCollecter( int n, SEXP types_) : 
            Collecter_Impl<RTYPE>(n), types(types_){}
        
        inline SEXP get(){
            Collecter_Impl<RTYPE>::data.attr("class") = types ;
            return Collecter_Impl<RTYPE>::data ;
        }
        
        inline bool compatible(SEXP x) const {
            String type = STRING_ELT(types,0) ;
            return Rf_inherits(x, type.get_cstring() ) ;    
        }
        
        inline bool can_promote(SEXP x) const {
            return false ;    
        }
        
        std::string describe() const {
            return collapse<STRSXP>(types) ;    
        }
        
    private:
        SEXP types ;
    } ;
    
    template <int RTYPE>
    class POSIXctCollecter : public Collecter_Impl<RTYPE>{
    public: 
        POSIXctCollecter( int n, SEXP tz_) : 
            Collecter_Impl<RTYPE>(n), tz(tz_){}
        
        inline SEXP get(){
            Collecter_Impl<RTYPE>::data.attr("class") = get_time_classes() ;
            if( !Rf_isNull(tz) ){
                Collecter_Impl<RTYPE>::data.attr("tzone") = tz ;    
            }
            return Collecter_Impl<RTYPE>::data ;
        }
        
        inline bool compatible(SEXP x) const {
            if( !Rf_inherits(x, "POSIXct" ) ) return false ;
            if( Rf_isNull(tz) ) return Rf_isNull( Rf_getAttrib(x, Rf_install("tzone") ) ) ;
            
            SEXP xtz = Rf_getAttrib(x, Rf_install("tzone" ) ) ;
            if( Rf_isNull( xtz ) ) return false ;
            
            return STRING_ELT(tz, 0) == STRING_ELT(xtz, 0 ) ;
        }
        
        inline bool can_promote(SEXP x) const {
            return false ;    
        }
        
        std::string describe() const {
            return collapse<STRSXP>(get_time_classes()) ;    
        }
        
    private:
        SEXP tz ;
        
    } ;
    
    class FactorCollecter : public Collecter {
    public:
        typedef dplyr_hash_map<SEXP,int> LevelsMap ;
        
        FactorCollecter( int n, CharacterVector levels): data(n, IntegerVector::get_na() ), levels_map() {
            int nlevels = levels.size() ;
            for( int i=0; i<nlevels; i++) levels_map[ levels[i] ] = i + 1;
        }
        
        bool is_factor_collecter() const{ 
            return true ;    
        }
        
        void collect( const SlicingIndex& index, SEXP v ){
            // here we can assume that v is a factor with the right levels
            // we however do not assume that they are in the same order
            IntegerVector source(v) ;
            CharacterVector levels = source.attr( "levels" ) ;
            
            SEXP* levels_ptr = Rcpp::internal::r_vector_start<STRSXP>(levels) ;
            int* source_ptr = Rcpp::internal::r_vector_start<INTSXP>(source) ;
            for( int i=0; i<index.size(); i++){
                if( source_ptr[i] == NA_INTEGER ){
                    data[ index[i] ] = NA_INTEGER ;
                } else {
                    SEXP x = levels_ptr[ source_ptr[i] - 1 ] ;
                    data[ index[i] ] = levels_map.find(x)->second ;
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
            return Rf_inherits( x, "factor" ) && has_same_levels_as(x) ;    
        }
        
        inline bool can_promote(SEXP x) const {
            return TYPEOF(x) == STRSXP || Rf_inherits( x, "factor" ) ;    
        }
        
        inline bool has_same_levels_as( SEXP x) const {
            CharacterVector levels_other = Rf_getAttrib( x, Rf_install( "levels" ) ) ;
            
            int nlevels = levels_other.size() ;
            if( nlevels != (int)levels_map.size() ) return false ;
            
            for( int i=0; i<nlevels; i++)
                if( ! levels_map.count(levels_other[i]) ) 
                    return false ;
            return true ;
        }
        
        inline std::string describe() const {
            return "factor" ;
        }
        
    private:
        IntegerVector data ;
        LevelsMap levels_map ;
    } ;
    
    template <>
    inline bool Collecter_Impl<LGLSXP>::can_promote( SEXP x) const {
        return ( TYPEOF(x) == INTSXP && ! Rf_inherits(x, "factor" ) ) || TYPEOF(x) == REALSXP ;
    }
    
    inline Collecter* collecter(SEXP model, int n){
        switch( TYPEOF(model) ){
        case INTSXP: 
            if( Rf_inherits(model, "factor") )
                return new FactorCollecter(n, Rf_getAttrib(model, Rf_install("levels") ) ) ;
            if( Rf_inherits(model, "Date") )
                return new TypedCollecter<INTSXP>(n, get_date_classes()) ;
            return new Collecter_Impl<INTSXP>(n) ;
        case REALSXP: 
            if( Rf_inherits( model, "POSIXct" ) )
                return new POSIXctCollecter<REALSXP>(n, Rf_getAttrib(model, Rf_install("tzone") ) ) ;
            if( Rf_inherits( model, "Date" ) )
                return new TypedCollecter<REALSXP>(n, get_date_classes()) ;
            return new Collecter_Impl<REALSXP>(n) ;
        case LGLSXP: return new Collecter_Impl<LGLSXP>(n) ;
        case STRSXP: return new Collecter_Impl<STRSXP>(n) ;
        case VECSXP: return new Collecter_Impl<VECSXP>(n) ;
        default: break ;
        }
        return 0 ;
    }
    
    inline Collecter* promote_collecter(SEXP model, int n, Collecter* previous){
        // handle the case where the previous collecter was a 
        // Factor collecter and model is a factor. when this occurs, we need to 
        // return a Collecter_Impl<STRSXP> because the factors don't have the 
        // same levels
        if( Rf_inherits( model, "factor" ) && previous->is_factor_collecter() ){
            Rf_warning( "Unequal factor levels: coercing to character" ) ;
            return new Collecter_Impl<STRSXP>(n) ;
        }
        
        switch( TYPEOF(model) ){
        case INTSXP: 
            if( Rf_inherits( model, "Date" ) )
                return new TypedCollecter<INTSXP>(n, get_date_classes() ) ;
            if( Rf_inherits(model, "factor") )
                return new FactorCollecter(n, Rf_getAttrib(model, Rf_install("levels") ) ) ;
            return new Collecter_Impl<INTSXP>(n) ;
        case REALSXP: 
            if( Rf_inherits( model, "POSIXct" ) )
                return new POSIXctCollecter<REALSXP>(n, Rf_getAttrib(model, Rf_install("tzone") ) ) ;
            if( Rf_inherits( model, "Date" ) )
                return new TypedCollecter<REALSXP>(n, get_date_classes() ) ;
            return new Collecter_Impl<REALSXP>(n) ;
        case LGLSXP: return new Collecter_Impl<LGLSXP>(n) ;
        case STRSXP: return new Collecter_Impl<STRSXP>(n) ;
        default: break ;
        }
        return 0 ;
    }
    
    
}

#endif
