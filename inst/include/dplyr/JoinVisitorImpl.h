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
            left(left_), right(right_), nleft(left.size()), nright(right.size()), n(nleft+nright)
        {
            make_orders() ;
        }

        inline int get_order(int i) const {
            if( i == NA_INTEGER ) return NA_INTEGER ;
            return (i>=0) ? orders[i] : orders[nleft-i-1] ;
        }

    private:
        const CharacterVector& left ;
        const CharacterVector& right ;
        int nleft, nright, n ;
        IntegerVector orders ;

        inline void make_orders(){
            CharacterVector big(n) ;
            CharacterVector::iterator it = big.begin() ;
            std::copy( left.begin(), left.end(), it ) ;
            std::copy( right.begin(), right.end(), it + nleft ) ;
            orders = CharacterVectorDifferentiator(big).get() ;
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

    CharacterVector get_uniques( const CharacterVector& left, const CharacterVector& right) ;
    IntegerVector match( const CharacterVector& s, const CharacterVector& levels) ;

    class JoinStringStringVisitor : public JoinVisitor {
    public:
        JoinStringStringVisitor( CharacterVector left, CharacterVector right) :
            uniques( get_uniques(left, right) ),
            i_left( match(left, uniques) ),
            i_right( match(right, uniques) ),
            int_visitor( i_left, i_right),
            p_uniques( internal::r_vector_start<STRSXP>(uniques) ),
            p_left( i_left.begin() ),
            p_right( i_right.begin() )
        {}

        inline size_t hash(int i) {
          return int_visitor.hash(i) ;
        }
        bool equal(int i, int j) {
          return int_visitor.equal(i,j) ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ){
            int n = set.size() ;
            CharacterVector res = no_init(n) ;

            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it = set.begin() ;
            for( int i=0; i<n; i++, ++it){
                res[i] = get(*it) ;
            }

            return res ;
        }

        inline SEXP subset( const std::vector<int>& indices ){
            int n = indices.size() ;
            CharacterVector res = no_init(n) ;

            for( int i=0; i<n; i++){
                res[i] = get(indices[i]) ;
            }

            return res ;
        }

        virtual void print(int i) {
            Rprintf( CHAR(get(i)) )  ;
        }

    private:
        CharacterVector uniques ;
        IntegerVector i_left, i_right ;
        JoinVisitorImpl<INTSXP,INTSXP> int_visitor ;
        SEXP* p_uniques ;
        int* p_left ;
        int* p_right ;

        inline SEXP get(int i){
            if( i >= 0 ){
                return ( i_left[i] == NA_INTEGER ) ? NA_STRING : p_uniques[ p_left[i] - 1] ;
            } else {
                return ( i_right[-i-1] == NA_INTEGER ) ? NA_STRING : p_uniques[ p_right[-i-1] - 1] ;
            }
        }
    } ;

    class JoinFactorStringVisitor : public JoinVisitor {
    public:
        JoinFactorStringVisitor( const IntegerVector& left_, const CharacterVector& right_ ) :
            left(left_),
            left_ptr(left.begin()),

            right(right_),
            uniques( get_uniques(left.attr("levels"), right) ) ,
            p_uniques( internal::r_vector_start<STRSXP>(uniques) ),

            i_right( match( right, uniques )),
            int_visitor( left, i_right )

        {}

        inline size_t hash(int i){
            return int_visitor.hash(i) ;
        }

        inline bool equal( int i, int j){
            return int_visitor.equal(i,j) ;
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
        int* left_ptr ;

        CharacterVector right ;
        CharacterVector uniques ;
        SEXP* p_uniques ;
        IntegerVector i_right ;

        JoinVisitorImpl<INTSXP,INTSXP> int_visitor ;

        inline SEXP get(int i){
            if( i>=0 ){
                if( left_ptr[i] == NA_INTEGER ) return NA_STRING ;
                return p_uniques[ left_ptr[i] - 1 ] ;
            } else {
                return p_uniques[ i_right[ -i-1 ] - 1] ;
            }
        }

    } ;

    class JoinStringFactorVisitor : public JoinVisitor {
    public:
        JoinStringFactorVisitor( const CharacterVector& left_, const IntegerVector& right_ ) :
            i_right(right_),
            uniques( get_uniques(i_right.attr("levels"), left_) ),
            p_uniques( internal::r_vector_start<STRSXP>(uniques) ),
            i_left( match(left_, uniques) ),

            int_visitor(i_left, i_right)
        {}

        inline size_t hash(int i){
            return int_visitor.hash(i) ;
        }

        inline bool equal( int i, int j){
            return int_visitor.equal(i,j) ;
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
        IntegerVector i_right ;
        CharacterVector uniques ;
        SEXP* p_uniques ;
        IntegerVector i_left ;

        JoinVisitorImpl<INTSXP, INTSXP> int_visitor ;

        inline SEXP get(int i){
            SEXP res ;

            if( i>=0 ){
                res = p_uniques[ i_left[i] - 1 ] ;
            } else {
                int index = -i-1 ;
                if( i_right[index] == NA_INTEGER ) {
                    res = NA_STRING ;
                } else {
                    res = p_uniques[ i_right[index] - 1 ] ;
                }
            }

            return res ;
        }

    } ;



    class POSIXctJoinVisitor : public JoinVisitorImpl<REALSXP,REALSXP> {
    public:
        typedef JoinVisitorImpl<REALSXP,REALSXP> Parent ;
        POSIXctJoinVisitor( NumericVector left, NumericVector right) :
          Parent(left, right),
          tzone(R_NilValue)
        {
          RObject tzone_left  = left.attr("tzone") ;
          RObject tzone_right = right.attr("tzone") ;
          if( tzone_left.isNULL() && tzone_right.isNULL() ) return ;

          if( tzone_left.isNULL() ) {
            tzone = tzone_right ;
          } else if( tzone_right.isNULL() ) {
            tzone = tzone_left ;
          } else {
            std::string s_left  = as<std::string>( tzone_left  ) ;
            std::string s_right = as<std::string>( tzone_right ) ;

            if( s_left == s_right){
              tzone = wrap(s_left) ;
            } else {
              tzone = wrap("UTC") ;
            }
          }
        }

        inline SEXP subset( const std::vector<int>& indices ){
          return promote( Parent::subset( indices ) ) ;
        }
        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
          return promote( Parent::subset(set)) ;
        }

    private:
        RObject tzone ;

        inline SEXP promote( NumericVector x){
          x.attr("class") = "POSIXct" ;
          if( !tzone.isNULL() ){
            x.attr("tzone") = tzone ;
          }
          return x ;
        }

    } ;

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
