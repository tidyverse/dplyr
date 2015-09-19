#ifndef dplyr_VectorVisitor_Impl_H
#define dplyr_VectorVisitor_Impl_H

namespace dplyr {

    template <typename Container>
    inline int output_size( const Container& container){
        return container.size() ;
    }

    template <>
    inline int output_size<LogicalVector>( const LogicalVector& container){
        return std::count( container.begin(), container.end(), TRUE ) ;
    }

    template <int RTYPE> std::string VectorVisitorType() ;
    template <> inline std::string VectorVisitorType<INTSXP>()  { return "integer" ; }
    template <> inline std::string VectorVisitorType<REALSXP>() { return "numeric" ; }
    template <> inline std::string VectorVisitorType<LGLSXP>()  { return "logical" ; }
    template <> inline std::string VectorVisitorType<STRSXP>()  { return "character" ; }
    template <> inline std::string VectorVisitorType<CPLXSXP>() { return "complex" ; }
    template <> inline std::string VectorVisitorType<VECSXP>()  { return "list" ; }

    /**
     * Implementations
     */
    template <int RTYPE>
    class VectorVisitorImpl : public VectorVisitor, public comparisons<RTYPE> {
    public:
        typedef comparisons<RTYPE> compare ;
        typedef Rcpp::Vector<RTYPE> VECTOR ;

        /**
         * The type of data : int, double, SEXP, Rcomplex
         */
        typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE ;

        /**
         * Hasher for that type of data
         */
        typedef boost::hash<STORAGE> hasher ;

        VectorVisitorImpl( const VECTOR& vec_ ) : vec(vec_) {}

        /**
         * implementations
         */
        size_t hash(int i) const {
            return hash_fun( vec[i] ) ;
        }
        inline bool equal(int i, int j) const {
            return compare::equal_or_both_na( vec[i], vec[j] ) ;
        }

        inline bool less(int i, int j) const {
            return compare::is_less( vec[i], vec[j] ) ;
        }

        inline bool equal_or_both_na(int i, int j) const {
            return compare::equal_or_both_na( vec[i], vec[j] ) ;
        }

        inline bool greater(int i, int j) const {
            return compare::is_greater( vec[i], vec[j] ) ;
        }

        inline std::string get_r_type() const {
            return VectorVisitorType<RTYPE>() ;
        }

        int size() const {
            return vec.size() ;
        }

        bool is_na( int i ) const {
            return VECTOR::is_na( vec[i] ) ;
        }

    protected:
        VECTOR vec ;
        hasher hash_fun ;

    } ;

    class FactorVisitor : public VectorVisitorImpl<INTSXP> {
    public:
        typedef VectorVisitorImpl<INTSXP> Parent ;

        FactorVisitor( const IntegerVector& vec_ ) : Parent(vec_){
                levels = vec.attr( "levels" ) ;
                levels_ptr = Rcpp::internal::r_vector_start<STRSXP>(levels) ;
        }

        inline bool equal(int i, int j) const {
            return vec[i] == vec[j] ;
        }

        inline bool less(int i, int j) const {
            return string_compare.is_less(
                vec[i] < 0 ? NA_STRING : levels_ptr[vec[i]],
                vec[j] < 0 ? NA_STRING : levels_ptr[vec[j]]
            ) ;
        }

        inline bool greater(int i, int j) const {
            return string_compare.is_greater(
                vec[i] < 0 ? NA_STRING : levels_ptr[vec[i]],
                vec[j] < 0 ? NA_STRING : levels_ptr[vec[j]]
            ) ;
        }

        inline std::string get_r_type() const {
            CharacterVector classes = Parent::vec.attr( "class" ) ;
            return collapse(classes) ;
        }

        bool is_compatible( VectorVisitor* other, std::stringstream& ss, const std::string& name ) const {
            return compatible( dynamic_cast<FactorVisitor*>(other), ss, name ) ;
        }

    private:

        inline bool compatible(FactorVisitor* other, std::stringstream& ss, const std::string& name ) const {
            CharacterVector levels_other = other->levels ;
            if( setdiff( levels, levels_other ).size() ){
                ss << "Factor levels not equal for column " << name ;
                return false ;
            }
            return true;
        }

        CharacterVector levels ;
        SEXP* levels_ptr ;
        comparisons<STRSXP> string_compare ;
    } ;


    template <>
    class VectorVisitorImpl<STRSXP> : public VectorVisitor {
    public:

        VectorVisitorImpl( const CharacterVector& vec_ ) :
            vec(vec_),
            orders( CharacterVectorOrderer(vec).get() )
        {}

        size_t hash(int i) const {
            return orders[i] ;
        }
        inline bool equal(int i, int j) const {
            return orders[i] == orders[j] ;
        }

        inline bool less(int i, int j) const {
            return orders[i] < orders[j] ;
        }

        inline bool equal_or_both_na(int i, int j) const {
            return orders[i] == orders[j] ;
        }

        inline bool greater(int i, int j) const {
            return orders[i] > orders[j] ;
        }

        inline std::string get_r_type() const {
            return VectorVisitorType<STRSXP>() ;
        }

        int size() const {
            return vec.size() ;
        }

        bool is_na( int i ) const {
            return CharacterVector::is_na(vec[i]) ;
        }

    protected:
        CharacterVector vec ;
        IntegerVector orders ;

    } ;

}

#endif
