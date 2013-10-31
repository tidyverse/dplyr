#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

namespace dplyr{
        
    template <int RTYPE>
    class JoinVisitorImpl : public JoinVisitor, public comparisons<RTYPE>{
    public:
        typedef Vector<RTYPE> Vec ;
        typedef comparisons<RTYPE> Compare ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef boost::hash<STORAGE> hasher ;
    
        JoinVisitorImpl( Rcpp::Vector<RTYPE> left_, Rcpp::Vector<RTYPE> right_ ) : 
            left(left_), right(right_){}
          
        inline size_t hash(int i){
            return hash_fun( get(i) ) ; 
        }
        
        inline bool equal( int i, int j){
            return Compare::equal_or_both_na( get(i), get(j) ) ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ){
            int n = indices.size() ;
            Vec res = no_init(n) ;
            for( int i=0; i<n; i++) res[i] = get( indices[i] ) ;
            return res ;
        }
        
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
            int n = set.size() ;
            Vec res = no_init(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) res[i] = get( *it ) ;
            return res ;
        }
        
        void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;    
        }
        
    protected:
        Vector<RTYPE> left, right ;
        hasher hash_fun ;
        
        inline STORAGE get(int i){
            return i>=0 ? left[i] : right[-i-1] ;    
        }
        
    } ;
              
    class StringLessPredicate : comparisons<STRSXP>{
    public:
        typedef SEXP value_type ;
        inline bool operator()( SEXP x, SEXP y){
            return comparisons::is_less(x, y) ;    
        }
    } ;  
    
    class JoinFactorVisitor : public JoinVisitorImpl<INTSXP> {
    public:
        typedef JoinVisitorImpl<INTSXP> Parent ;
        
        JoinFactorVisitor( const IntegerVector& left, const IntegerVector& right ) : 
            Parent(left, right), 
            left_levels_ptr( internal::r_vector_start<STRSXP>( left.attr("levels") ) ) ,
            right_levels_ptr( internal::r_vector_start<STRSXP>( right.attr("levels") ) )
            {}
        
        inline size_t hash(int i){
            return string_hash( get(i) ) ; 
        }
        
        void print(int i){
            Rcpp::Rcout << get(i) << " :: " << toString<STRSXP>(get(i)) << std::endl ;    
        }
        
        inline bool equal( int i, int j){
            return string_compare.equal_or_both_na( get(i), get(j) ) ;
        }
        
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
            int n = set.size() ;
            Vec res = no_init(n) ;
            
            typedef std::set<SEXP, StringLessPredicate> LevelsSet ;
            LevelsSet levels ;
            std::vector<SEXP> strings(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it = set.begin() ;
            for( int i=0; i<n; i++, ++it){
                SEXP s = get(*it) ;
                levels.insert( s );
                strings[i] = s ;
            }
            boost::unordered_map<SEXP,int> invmap ;
            
            int nlevels = levels.size() ;
            LevelsSet::const_iterator lit = levels.begin() ;
            CharacterVector levels_vector( nlevels );
            for( int i=0; i<nlevels; i++, ++lit){
                invmap[*lit] = i+1 ;
                levels_vector[i] = *lit ;
            }
            
            for( int i=0; i<n; i++){
                res[i] = invmap[strings[i]] ;
            }
            res.attr( "class" )  = Parent::left.attr("class" ) ;
            res.attr( "levels" ) = levels_vector ;
            
            return res ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ){
            int n = indices.size() ;
            Vec res = no_init(n) ;
            
            typedef std::set<SEXP, StringLessPredicate> LevelsSet ;
            LevelsSet levels ;
            std::vector<SEXP> strings(n) ;
            for( int i=0; i<n; i++){
                SEXP s = get(indices[i]) ;
                levels.insert( s );
                strings[i] = s ;
            }
            boost::unordered_map<SEXP,int> invmap ;
            
            int nlevels = levels.size() ;
            LevelsSet::const_iterator lit = levels.begin() ;
            CharacterVector levels_vector( nlevels );
            for( int i=0; i<nlevels; i++, ++lit){
                invmap[*lit] = i+1 ;
                levels_vector[i] = *lit ;
            }
            
            for( int i=0; i<n; i++){
                res[i] = invmap[ strings[i] ] ;
            }
            res.attr( "class" )  = Parent::left.attr("class" ) ;
            res.attr( "levels" ) = levels_vector ;
            
            return res ;
        }
        
            
    private:
        SEXP* left_levels_ptr ;
        SEXP* right_levels_ptr ;
        comparisons<STRSXP> string_compare ;
        boost::hash<SEXP> string_hash ;
    
        inline SEXP get(int i){
            return i>=0 ? left_levels_ptr[ left[i] - 1] : right_levels_ptr[right[-i-1] - 1] ;    
        }
        
    } ;
    
    template <typename Class, typename JoinVisitorImpl> 
    class PromoteClassJoinVisitor : public JoinVisitorImpl {
    public:
        typedef typename JoinVisitorImpl::Vec Vec ;
        
        PromoteClassJoinVisitor( const Vec& left, const Vec& right) : JoinVisitorImpl(left, right){}
        
        inline SEXP subset( const std::vector<int>& indices ){
            return promote( JoinVisitorImpl::subset( indices) ) ; 
        }
        
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
            return promote( JoinVisitorImpl::subset( set) ) ;
        }
        
    private:
        inline SEXP promote( Vec vec){
            vec.attr( "class" ) = JoinVisitorImpl::left.attr( "class" );
            return vec ;
        }
    } ;
    
    #define PROMOTE_JOIN_VISITOR(__CLASS__)                                                   \
    class __CLASS__ : public PromoteClassJoinVisitor<__CLASS__, JoinVisitorImpl<REALSXP> >{\
    public:                                                                              \
        __CLASS__( const NumericVector& left_, const NumericVector& right_) :   \
        PromoteClassJoinVisitor(left_, right_){}              \
    } ;
    PROMOTE_JOIN_VISITOR(DateJoinVisitor)
    PROMOTE_JOIN_VISITOR(POSIXctJoinVisitor)
    
    inline JoinVisitor* join_visitor( SEXP left, SEXP right ){
        if( TYPEOF(left) != TYPEOF(right) ) 
            stop( "cannot create join visitor from incompatible types" ) ;
        switch( TYPEOF(left) ){
            case INTSXP:
                if( Rf_inherits( left, "factor" ) )
                    return new JoinFactorVisitor(left, right) ;
                return new JoinVisitorImpl<INTSXP> ( left, right ) ;
            case REALSXP: 
                if( Rf_inherits( left, "Date" ) )
                    return new DateJoinVisitor(left, right ) ;
                if( Rf_inherits( left, "POSIXct" ) )
                    return new POSIXctJoinVisitor(left, right ) ;
                
                return new JoinVisitorImpl<REALSXP>( left, right ) ;
            case LGLSXP:  return new JoinVisitorImpl<LGLSXP> ( left, right ) ;
            case STRSXP:  return new JoinVisitorImpl<STRSXP> ( left, right ) ;
            default: break ;
        }
        return 0 ;
    }

}

#endif

