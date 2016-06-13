#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

namespace dplyr{

    CharacterVector get_uniques( const CharacterVector& left, const CharacterVector& right) ;

    template <int LHS_RTYPE, int RHS_RTYPE>
    class JoinVisitorImpl : public JoinVisitor, public comparisons_different<LHS_RTYPE, RHS_RTYPE>{
    public:
        typedef Vector<LHS_RTYPE> LHS_Vec ;
        typedef Vector<RHS_RTYPE> RHS_Vec ;

        typedef typename Rcpp::traits::storage_type<LHS_RTYPE>::type LHS_STORAGE ;
        typedef typename Rcpp::traits::storage_type<RHS_RTYPE>::type RHS_STORAGE ;

        typedef boost::hash<LHS_STORAGE> LHS_hasher ;
        typedef boost::hash<RHS_STORAGE> RHS_hasher ;

        JoinVisitorImpl( LHS_Vec left_, RHS_Vec right_ ) : left(left_), right(right_){
          check_attribute_compatibility(left, right) ;
        }

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

        LHS_Vec left ;
        RHS_Vec right ;
        LHS_hasher LHS_hash_fun ;
        RHS_hasher RHS_hash_fun ;

    } ;

    template <typename Visitor>
    class Subsetter {
    public:
        typedef typename Visitor::Vec Vec ;

        Subsetter( const Visitor& v_) : v(v_){} ;

        inline SEXP subset( const std::vector<int>& indices ) {
          int n = indices.size() ;
          Vec res = no_init(n) ;
          for( int i=0; i<n; i++) {
              res[i] = v.get(indices[i]) ;
          }
          return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            int n = set.size() ;
            Vec res = no_init(n) ;
            VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin() ;
            for( int i=0; i<n; i++, ++it) {
                res[i] = v.get(*it) ;
            }
            return res ;
        }
    private:
        const Visitor& v ;
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
            RObject res = Subsetter<JoinVisitorImpl>(*this).subset(indices ) ;
            copy_most_attributes(res, left) ;
            return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            RObject res = Subsetter<JoinVisitorImpl>(*this).subset(set) ;
            copy_most_attributes(res, left) ;
            return res ;
        }

        inline STORAGE get(int i) const {
            return i >= 0 ? left[i] : right[-i-1] ;
        }

    protected:
        Vec left, right ;
        hasher hash_fun ;

    } ;

    class JoinFactorFactorVisitor : public JoinVisitor {
    public:
        typedef CharacterVector Vec ;

        JoinFactorFactorVisitor( const IntegerVector& left_, const IntegerVector& right_ ) :
            left(left_),
            right(right_),
            left_levels (left.attr("levels")),
            right_levels(right.attr("levels")),
            uniques( get_uniques(left_levels, right_levels) ),
            left_match ( match( left_levels, uniques) ),
            right_match( match( right_levels, uniques) )
        {}

        inline size_t hash(int i){
            return hash_fun( get(i) ) ;
        }

        inline bool equal( int i, int j){
            return get(i) == get(j) ;
        }

        inline SEXP subset( const std::vector<int>& indices ) {
            return Subsetter<JoinFactorFactorVisitor>(*this).subset(indices) ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            return Subsetter<JoinFactorFactorVisitor>(*this).subset(set) ;
        }

        inline SEXP get(int i) const {
            if( i >= 0){
              int pos = left[i] ;
              return (pos == NA_INTEGER) ? NA_STRING : SEXP( uniques[left_match[pos-1] - 1] ) ;
            } else {
              int pos = right[-i-1] ;
              return (pos == NA_INTEGER) ? NA_STRING : SEXP( uniques[right_match[ pos -1 ] - 1] ) ;
            }
        }

    private:
        IntegerVector left, right ;
        CharacterVector left_levels, right_levels ;
        CharacterVector uniques ;
        IntegerVector left_match, right_match ;
        boost::hash<SEXP> hash_fun ;

    } ;

    class JoinStringStringVisitor : public JoinVisitor {
    public:
        typedef CharacterVector Vec ;

        JoinStringStringVisitor( CharacterVector left_, CharacterVector right) :
            left(left_),
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

        inline SEXP subset( const std::vector<int>& indices ) {
            RObject res = Subsetter<JoinStringStringVisitor>(*this).subset(indices) ;
            copy_most_attributes( res, left) ;
            return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            RObject res = Subsetter<JoinStringStringVisitor>(*this).subset(set) ;
            copy_most_attributes(res, left) ;
            return res ;
        }

        inline SEXP get(int i) const {
            if( i >= 0 ){
                return ( i_left[i] == NA_INTEGER ) ? NA_STRING : p_uniques[ p_left[i] - 1] ;
            } else {
                return ( i_right[-i-1] == NA_INTEGER ) ? NA_STRING : p_uniques[ p_right[-i-1] - 1] ;
            }
        }

    private:
        CharacterVector left ;
        CharacterVector uniques ;
        IntegerVector i_left, i_right ;
        JoinVisitorImpl<INTSXP,INTSXP> int_visitor ;
        SEXP* p_uniques ;
        int* p_left ;
        int* p_right ;


    } ;

    class JoinFactorStringVisitor : public JoinVisitor {
    public:
        typedef CharacterVector Vec ;

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

        inline SEXP subset( const std::vector<int>& indices ) {
            RObject res = Subsetter<JoinFactorStringVisitor>(*this).subset(indices) ;
            // copy_most_attributes(res, left) ;
            return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            RObject res = Subsetter<JoinFactorStringVisitor>(*this).subset(set) ;
            // copy_most_attributes(res, left) ;
            return res ;
        }

        inline SEXP get(int i) const {
            if( i>=0 ){
                if( left_ptr[i] == NA_INTEGER ) return NA_STRING ;
                return p_uniques[ left_ptr[i] - 1 ] ;
            } else {
                return p_uniques[ i_right[ -i-1 ] - 1] ;
            }
        }

    private:
        IntegerVector left ;
        int* left_ptr ;

        CharacterVector right ;
        CharacterVector uniques ;
        SEXP* p_uniques ;
        IntegerVector i_right ;

        JoinVisitorImpl<INTSXP,INTSXP> int_visitor ;


    } ;

    class JoinStringFactorVisitor : public JoinVisitor {
    public:
        typedef CharacterVector Vec ;

        JoinStringFactorVisitor( const CharacterVector& left_, const IntegerVector& right_ ) :
            left(left_),
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

        inline SEXP subset( const std::vector<int>& indices ) {
            RObject res = Subsetter<JoinStringFactorVisitor>(*this).subset(indices) ;
            // copy_most_attributes(res, left) ;
            return res;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            RObject res = Subsetter<JoinStringFactorVisitor>(*this).subset(set) ;
            // copy_most_attributes(res, left) ;
            return res ;
        }

        inline SEXP get(int i) const {
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

    private:
        CharacterVector left ;
        IntegerVector i_right ;
        CharacterVector uniques ;
        SEXP* p_uniques ;
        IntegerVector i_left ;

        JoinVisitorImpl<INTSXP, INTSXP> int_visitor ;

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
          x.attr("class") = Rcpp::CharacterVector::create("POSIXct", "POSIXt") ;
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
        typedef NumericVector Vec ;
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
            NumericVector res = Subsetter<DateJoinVisitor>(*this).subset(indices) ;
            res.attr("class") = "Date" ;
            return res ;
        }

        inline SEXP subset( const VisitorSetIndexSet<DataFrameJoinVisitors>& set ) {
            NumericVector res = Subsetter<DateJoinVisitor>(*this).subset(set) ;
            res.attr("class") = "Date" ;
            return res ;
        }

        inline double get( int i) const {
            if( i>= 0 ){
                return left->get(i) ;
            } else {
                return right->get(-i-1) ;
            }
        }

    private:
        DateJoinVisitorGetter* left ;
        DateJoinVisitorGetter* right ;
        hasher hash_fun ;

        DateJoinVisitor( const DateJoinVisitor& ) ;


    } ;


}

#endif
