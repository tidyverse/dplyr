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

    class JoinStringOrderer {
    public:
        JoinStringOrderer( const CharacterVector& left_, const CharacterVector& right_ ) :
            left(left_), right(right_), nleft(left.size()), nright(right.size()), n(nleft+nright), n_na(0)
        {
            make_orders() ;
        }

        inline int get_order(int i) const {
            if( i == NA_INTEGER ) return NA_INTEGER ;
            int val = (i>=0) ? orders[i] : orders[nleft-i-1] ;
            if( val > n - n_na ) val= NA_INTEGER ;
            return val ;
        }

    private:
        const CharacterVector& left ;
        const CharacterVector& right ;
        int nleft, nright, n ;
        IntegerVector orders ;
        int n_na ;

        inline void make_orders(){
            CharacterVector big(n) ;
            CharacterVector::iterator it = big.begin() ;
            std::copy( left.begin(), left.end(), it ) ;
            std::copy( right.begin(), right.end(), it + nleft ) ;
            orders = CharacterVectorOrderer(big).get() ;
            n_na = std::count( big.begin(), big.end(), NA_STRING ) ;
        }

    } ;


    template <>
    class JoinVisitorImpl<STRSXP,STRSXP> : public JoinVisitor, public comparisons<STRSXP>{
    public:
        typedef comparisons<STRSXP> Compare ;

        typedef CharacterVector Vec ;
        typedef SEXP STORAGE ;
        typedef boost::hash<int> hasher ;

        JoinVisitorImpl( CharacterVector left_, CharacterVector right_ ) :
            left(left_), right(right_), orderer(left,right)
        {}

        inline size_t hash(int i){
            return hash_fun( orderer.get_order(i) ) ;
        }

        inline bool equal( int i, int j) {
            return orderer.get_order(i) == orderer.get_order(j) ;
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
        JoinStringOrderer orderer ;

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
            left_levels( left_.attr("levels") ),
            left_factor_ptr(Rcpp::internal::r_vector_start<STRSXP>(left_levels) ),
            right_ptr(Rcpp::internal::r_vector_start<STRSXP>(right_)),
            orderer(left_levels, right)
        {}

        inline size_t hash(int i){
            return hash_fun( orderer.get_order(get_pos(i)) ) ;
        }

        inline bool equal( int i, int j){
            return orderer.get_order(get_pos(i)) == orderer.get_order(get_pos(j)) ;
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
        IntegerVector::const_iterator left_ptr ;
        CharacterVector left_levels ;
        SEXP* left_factor_ptr ;
        SEXP* right_ptr ;
        boost::hash<int> hash_fun ;
        JoinStringOrderer orderer ;

        inline SEXP get(int i){
            if( i>=0 ){
                if( left_ptr[i] == NA_INTEGER ) return NA_STRING ;
                return left_factor_ptr[ left_ptr[i] - 1 ] ;
            } else {
                return right_ptr[ -i-1 ] ;
            }
        }

        inline int get_pos( int i ){
            if( i>= 0 ) {
                if( left_ptr[i] == NA_INTEGER ) return NA_INTEGER ;
                return left_ptr[i] - 1 ;
            }
            return i ;
        }

    } ;

    class JoinStringFactorVisitor : public JoinVisitor {
    public:
        JoinStringFactorVisitor( const CharacterVector& left_, const IntegerVector& right_ ) :
            left(left_),
            right(right_),
            right_ptr(right_.begin()),
            right_levels(right_.attr("levels")),
            right_factor_ptr(Rcpp::internal::r_vector_start<STRSXP>(right_levels) ),
            left_ptr(Rcpp::internal::r_vector_start<STRSXP>(left_)),
            orderer(left, right_levels)
        {}

        inline size_t hash(int i){
            return hash_fun( orderer.get_order(get_pos(i)) ) ;
        }

        inline bool equal( int i, int j){
            return orderer.get_order(get_pos(i)) == orderer.get_order(get_pos(j)) ;
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
        IntegerVector::const_iterator  right_ptr ;
        CharacterVector right_levels ;
        SEXP* right_factor_ptr ;
        SEXP* left_ptr ;
        boost::hash<int> hash_fun ;
        JoinStringOrderer orderer ;

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

        inline int get_pos(int i) const {
            if( i>=0 ) {
                return i ;
            }
            int index = right_ptr[-i-1] ;
            if( index == NA_INTEGER ) return NA_INTEGER ;
            return - index ;
        }


    } ;


    class JoinFactorFactorVisitor : public JoinVisitorImpl<INTSXP, INTSXP> {
    public:
        typedef JoinVisitorImpl<INTSXP,INTSXP> Parent ;

        JoinFactorFactorVisitor( const IntegerVector& left, const IntegerVector& right ) :
            Parent(left, right),
            left_levels(left.attr("levels")),
            right_levels(right.attr("levels")),
            left_levels_ptr( Rcpp::internal::r_vector_start<STRSXP>( left_levels ) ) ,
            right_levels_ptr( Rcpp::internal::r_vector_start<STRSXP>( right_levels ) ),
            orderer(left_levels, right_levels)
            {}

        inline size_t hash(int i){
            return hash_fun( orderer.get_order(get_pos(i)) ) ;
        }

        void print(int i){
            Rcpp::Rcout << get(i) << " :: " << toString<STRSXP>(get(i)) << std::endl ;
        }

        inline bool equal( int i, int j){
            return orderer.get_order(get_pos(i)) ==  orderer.get_order(get_pos(j)) ;
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
        CharacterVector left_levels, right_levels ;
        SEXP* left_levels_ptr ;
        SEXP* right_levels_ptr ;
        JoinStringOrderer orderer ;
        boost::hash<int> hash_fun ;

        inline SEXP get(int i){
            if( i >= 0 ){
                return ( left[i] == NA_INTEGER ) ? NA_STRING : left_levels_ptr[ left[i] - 1] ;
            } else {
                return ( right[-i-1] == NA_INTEGER ) ? NA_STRING : right_levels_ptr[right[-i-1] - 1] ;
            }
        }

        inline int get_pos(int i) const {
            if( i >= 0 ){
                if( left[i] == NA_INTEGER ) return NA_INTEGER ;
                return left[i] - 1 ;
            } else {
                if( right[-i-1] == NA_INTEGER ) return NA_INTEGER ;
                return - right[-i-1] ;
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
            copy_most_attributes(vec,JoinVisitorImpl::left ) ;
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
    PROMOTE_JOIN_VISITOR(POSIXctJoinVisitor)

    JoinVisitor* join_visitor( SEXP, SEXP, const std::string&, const std::string&, bool warn ) ;

    class DateJoinVisitorGetter {
    public:
      virtual ~DateJoinVisitorGetter(){} ;
      virtual double get(int i) = 0 ;
    } ;

    template <int RTYPE>
    class DateJoinVisitorGetterImpl : public DateJoinVisitorGetter {
    public:
        DateJoinVisitorGetterImpl( SEXP x) : data(x){}

        inline double get(int i){
          return (double) data[i] ;
        }

    private:
        Vector<RTYPE> data ;
    } ;

    class DateJoinVisitor : public JoinVisitor, public comparisons<REALSXP>{
    public:
        typedef comparisons<REALSXP> Compare ;
        typedef boost::hash<double> hasher ;

        DateJoinVisitor( SEXP lhs, SEXP rhs)
        {
            if( TYPEOF(lhs) == INTSXP ) {
              left = new DateJoinVisitorGetterImpl<INTSXP>(lhs) ;
            } else if( TYPEOF(lhs) == REALSXP) {
              left = new DateJoinVisitorGetterImpl<REALSXP>(lhs) ;
            } else {
              stop("Date objects should be represented as integer or numeric") ;
            }

            if( TYPEOF(rhs) == INTSXP) {
              right = new DateJoinVisitorGetterImpl<INTSXP>(rhs) ;
            } else if( TYPEOF(rhs) == REALSXP) {
              right = new DateJoinVisitorGetterImpl<REALSXP>(rhs) ;
            } else {
              stop("Date objects should be represented as integer or numeric") ;
            }

        }

        ~DateJoinVisitor(){
          delete left ;
          delete right;
        }

        inline size_t hash(int i) {
            return hash_fun( get(i) ) ;
        }
        inline bool equal(int i, int j) {
            return Compare::equal_or_both_na(
                get(i), get(j)
            ) ;
        }

        inline SEXP subset( const std::vector<int>& indices ) {
            int n = indices.size() ;
            NumericVector res = no_init(n) ;
            for( int i=0; i<n; i++) {
                res[i] = get(indices[i]) ;
            }
            res.attr("class") = "Date" ;
            return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            NumericVector res = no_init(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = get(*it) ;
            }
            res.attr("class") = "Date" ;
            return res ;
        }

        inline void print(int i){
            Rcpp::Rcout << get(i) << std::endl ;
        }

    private:
        DateJoinVisitorGetter* left ;
        DateJoinVisitorGetter* right ;
        hasher hash_fun ;

        DateJoinVisitor( const DateJoinVisitor& ) ;

        inline double get( int i) {
            if( i>= 0 ){
                return left->get(i) ;
            } else {
                return right->get(-i-1) ;
            }
        }
    } ;


}

#endif
