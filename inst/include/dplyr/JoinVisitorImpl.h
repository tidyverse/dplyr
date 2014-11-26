#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

namespace dplyr{
       
    template <int LHS_RTYPE, int RHS_RTYPE>
    class JoinVisitorImpl : public JoinVisitor, public comparisons_different<LHS_RTYPE, RHS_RTYPE>{
    public:
        typedef Vector<LHS_RTYPE> LHS_Vec ;
        typedef Vector<RHS_RTYPE> RHS_Vec ;
        
        typedef typename Rcpp::traits::storage_type<LHS_RTYPE>::type LHS_STORAGE ;
        typedef typename Rcpp::traits::storage_type<RHS_RTYPE>::type RHS_STORAGE ;
        
        typedef boost::hash<LHS_STORAGE> LHS_hasher ;
        typedef boost::hash<RHS_STORAGE> RHS_hasher ;
    
        JoinVisitorImpl( LHS_Vec left_, RHS_Vec right_ ) : left(left_), right(right_){}
          
        size_t hash(int i) ;
        
        inline bool equal( int i, int j) {
            if( i>=0 && j>=0 ) {
                return comparisons<LHS_RTYPE>().equal_or_both_na( left[i], left[j] ) ;
            } else if( i < 0 && j < 0 ) {
                return comparisons<LHS_RTYPE>().equal_or_both_na( right[-i-1], right[-j-1] ) ;
            } else if( i >= 0 && j < 0) {
                return comparisons_different<LHS_RTYPE,RHS_RTYPE>().equal_or_both_na( left[i], right[-j-1] ) ;
            } else {
                return comparisons_different<RHS_RTYPE,LHS_RTYPE>().equal_or_both_na( right[-i-1], left[j] ) ;
            }
        }
        
        inline SEXP subset( const std::vector<int>& indices ); 
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) ;
        
        inline void print(int i){
            if( i >= 0){
                Rcpp::Rcout << left[i] << std::endl ;
            } else {
                Rcpp::Rcout << right[-i-1] << std::endl ;
            }
        }
        
        LHS_Vec left ;
        RHS_Vec right ;
        LHS_hasher LHS_hash_fun ;
        RHS_hasher RHS_hash_fun ;
        
    } ;   
    
    template <int RTYPE>
    class JoinVisitorImpl<RTYPE,RTYPE> : public JoinVisitor, public comparisons<RTYPE>{
    public:
        typedef comparisons<RTYPE> Compare ;
        
        typedef Vector<RTYPE> Vec ;
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;
        typedef boost::hash<STORAGE> hasher ;
    
        JoinVisitorImpl( Vec left_, Vec right_ ) : left(left_), right(right_){}
          
        inline size_t hash(int i){
            return hash_fun( get(i) ) ; 
        }
        
        inline bool equal( int i, int j) {
            return Compare::equal_or_both_na(
                get(i), get(j) 
            ) ;  
        }
        
        inline SEXP subset( const std::vector<int>& indices ) {
            int n = indices.size() ;
            Vec res = no_init(n) ;
            for( int i=0; i<n; i++) {
                res[i] = get(indices[i]) ;
            }
            return res ;
        }
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            Vec res = no_init(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = get(*it) ;
            }
            return res ;    
        }
        
        inline void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;
        }
        
        
    protected:
        Vec left, right ;
        hasher hash_fun ;
        
        inline STORAGE get(int i){
            return i >= 0 ? left[i] : right[-i-1] ;    
        }
        
    } ; 
    
    template <>
    class JoinVisitorImpl<STRSXP,STRSXP> : public JoinVisitor, public comparisons<STRSXP>{
    public:
        typedef comparisons<STRSXP> Compare ;
        
        typedef CharacterVector Vec ;
        typedef SEXP STORAGE ;
        typedef boost::hash<STORAGE> hasher ;
    
        JoinVisitorImpl( CharacterVector left_, CharacterVector right_ ) : left(left_), right(right_){
            check_all_same_encoding(left,right) ;
            // check_all_utf8(left); 
            // check_all_utf8(right);
        }
              
        inline size_t hash(int i){
            return hash_fun( get(i) ) ; 
        }
        
        inline bool equal( int i, int j) {
            return Compare::equal_or_both_na(
                get(i), get(j) 
            ) ;  
        }
        
        inline SEXP subset( const std::vector<int>& indices ) {
            int n = indices.size() ;
            Vec res = no_init(n) ;
            for( int i=0; i<n; i++) {
                res[i] = get(indices[i]) ;
            }
            return res ;
        }
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            Vec res = no_init(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = get(*it) ;
            }
            return res ;    
        }
        
        inline void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;
        }
        
        
    protected:
        CharacterVector left, right ;
        hasher hash_fun ;
        
        inline STORAGE get(int i){
            return i >= 0 ? left[i] : right[-i-1] ;    
        }
        
    } ;
    
    class StringLessPredicate : comparisons<STRSXP>{
    public:
        typedef SEXP value_type ;
        inline bool operator()( SEXP x, SEXP y){
            return is_less(x, y) ;    
        }
    } ;  
    
    class JoinFactorStringVisitor : public JoinVisitor {
    public:
        JoinFactorStringVisitor( const IntegerVector& left_, const CharacterVector& right_ ) : 
            left(left_), 
            right(right_),
            left_ptr(left_.begin()), 
            left_factor_ptr(Rcpp::internal::r_vector_start<STRSXP>(left_.attr("levels")) ), 
            right_ptr(Rcpp::internal::r_vector_start<STRSXP>(right_))
        {
            check_all_same_encoding(right_, left_.attr("levels")) ; 
            // check_all_utf8(right_) ;
            // check_all_utf8(left_.attr("levels")) ;
        }
            
        inline size_t hash(int i){
            return string_hash( get(i) ) ;
        }
        
        inline bool equal( int i, int j){
            SEXP left  = get(i) ;
            SEXP right = get(j) ;
            return left == right ;     
        }
        
        inline void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ) {
            int n = indices.size() ;
            CharacterVector res(n) ;
            for( int i=0; i<n; i++) {
                res[i] = get(indices[i]) ;
            }
            return res ;
        }
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            CharacterVector res(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = get(*it) ;
            }
            return res ;    
        }
        
    private:
        IntegerVector left ;
        CharacterVector right ;
        int* left_ptr ;
        SEXP* left_factor_ptr ;
        SEXP* right_ptr ;
        boost::hash<SEXP> string_hash ;
    
        inline SEXP get(int i){
            if( i>=0 ){
                if( left_ptr[i] == NA_INTEGER ) return NA_STRING ;
                return left_factor_ptr[ left_ptr[i] - 1 ] ;
            } else {
                return right_ptr[ -i-1 ] ;    
            }
        }
        
    } ;
    
    class JoinStringFactorVisitor : public JoinVisitor {
    public:
        JoinStringFactorVisitor( const CharacterVector& left_, const IntegerVector& right_ ) : 
            left(left_), 
            right(right_),
            right_ptr(right_.begin()), 
            right_factor_ptr(Rcpp::internal::r_vector_start<STRSXP>(right_.attr("levels")) ), 
            left_ptr(Rcpp::internal::r_vector_start<STRSXP>(left_))
        {
            check_all_same_encoding(left_,right_.attr("levels")) ;
        }
                
        inline size_t hash(int i){ 
            return string_hash( get(i) ) ;
        }
        
        inline bool equal( int i, int j){
            SEXP left = get(i) ;
            SEXP right = get(j) ;
            return left == right ;     
        }
        
        inline void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ) {
            int n = indices.size() ;
            CharacterVector res(n) ;
            for( int i=0; i<n; i++) {
                res[i] = get(indices[i]) ;
            }
            return res ;
        }
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            CharacterVector res(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = get(*it) ;
            }
            return res ;    
        }
        
    private:
        CharacterVector left ;
        IntegerVector right ;
        int*  right_ptr ;
        SEXP* right_factor_ptr ;
        SEXP* left_ptr ;
        boost::hash<SEXP> string_hash ;
    
        inline SEXP get(int i){
            SEXP res ;
            
            if( i>=0 ){
                res = left_ptr[i] ;
            } else {
                int index = -i-1 ;
                if( right_ptr[index] == NA_INTEGER ) {
                    res = NA_STRING ;
                } else {
                    res = right_factor_ptr[ right_ptr[index] - 1 ] ;
                }
            }
            
            return res ;
        }
        
    } ;
    
    
    class JoinFactorFactorVisitor : public JoinVisitorImpl<INTSXP, INTSXP> {
    public:
        typedef JoinVisitorImpl<INTSXP,INTSXP> Parent ;
        
        JoinFactorFactorVisitor( const IntegerVector& left, const IntegerVector& right ) : 
            Parent(left, right), 
            left_levels_ptr( Rcpp::internal::r_vector_start<STRSXP>( left.attr("levels") ) ) ,
            right_levels_ptr( Rcpp::internal::r_vector_start<STRSXP>( right.attr("levels") ) )
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
            CharacterVector res(n) ;
            
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it = set.begin() ;
            for( int i=0; i<n; i++, ++it){
                res[i] = get(*it) ;
            }
            
            return res ;
        }
        
        inline SEXP subset( const std::vector<int>& indices ){
            int n = indices.size() ;
            CharacterVector res(n) ;
            
            for( int i=0; i<n; i++){
                res[i] = get(indices[i]) ;
            }

            return res ;
        }
        
            
    private:
        SEXP* left_levels_ptr ;
        SEXP* right_levels_ptr ;
        comparisons<STRSXP> string_compare ;
        boost::hash<SEXP> string_hash ;
    
        inline SEXP get(int i){
            if( i >= 0 ){
                return ( left[i] == NA_INTEGER ) ? NA_STRING : left_levels_ptr[ left[i] - 1] ;
            } else {
                return ( right[-i-1] == NA_INTEGER ) ? NA_STRING : right_levels_ptr[right[-i-1] - 1] ;                  
            }
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
    class __CLASS__ : public PromoteClassJoinVisitor<__CLASS__, JoinVisitorImpl<REALSXP,REALSXP> >{   \
    public:                                                                                   \
        typedef PromoteClassJoinVisitor<__CLASS__, JoinVisitorImpl<REALSXP,REALSXP> > Parent ;        \
        __CLASS__( const NumericVector& left_, const NumericVector& right_) :                 \
            Parent(left_, right_){}                                                           \
    } ;
    PROMOTE_JOIN_VISITOR(DateJoinVisitor)
    PROMOTE_JOIN_VISITOR(POSIXctJoinVisitor)
    
    JoinVisitor* join_visitor( SEXP, SEXP, const std::string&, const std::string&, bool warn ) ;
}

#endif

