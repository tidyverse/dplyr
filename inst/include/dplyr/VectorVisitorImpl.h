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
        
        inline SEXP subset( const Rcpp::IntegerVector& index) const {
            return subset_int_index( index) ;    
        }
        
        inline SEXP subset( const std::vector<int>& index) const {
            return subset_int_index( index) ;    
        }
        
        inline SEXP subset( const ChunkIndexMap& map ) const {
            int n = output_size(map) ;
            VECTOR out = Rcpp::no_init(n) ;
            ChunkIndexMap::const_iterator it = map.begin(); 
            for( int i=0; i<n; i++, ++it)
                out[i] = vec[ it->first ] ;
            copy_most_attributes(out, vec) ;
            return out ;
        }
        
        inline SEXP subset( const Rcpp::LogicalVector& index ) const {
            int n = output_size(index) ;
            VECTOR out = Rcpp::no_init(n) ;
            for( int i=0, k=0; k<n; k++, i++ ) {
                while( index[i] != TRUE ) i++; 
                out[k] = vec[i] ;
            }
            copy_most_attributes(out, vec) ;
            return out ;
        }
        
        inline SEXP subset( EmptySubset ) const {
            VECTOR out(0) ;
            copy_most_attributes(out, vec) ;
            return out ;
        }
    
        inline std::string get_r_type() const {
            return VectorVisitorType<RTYPE>() ;    
        }
        
        int size() const { 
            return vec.size() ; 
        }
        
    protected: 
        VECTOR vec ;
        hasher hash_fun ;
        
        template <typename Container>
        inline SEXP subset_int_index( const Container& index ) const {
            int n = output_size(index) ;
            VECTOR out = Rcpp::no_init(n) ;
            for( int i=0; i<n; i++){
                if( index[i] < 0 ){
                    out[i] = VECTOR::get_na() ;
                } else {
                    out[i] = vec[ index[i] ] ;
                }
            }
            copy_most_attributes(out, vec) ;
            return out ;
        }
        
    } ;
    
    template <>
    template <typename Container>
    SEXP VectorVisitorImpl<VECSXP>::subset_int_index( const Container& index ) const {
        int n = output_size(index) ;
        List out(n) ;
        for( int i=0; i<n; i++) 
            out[i] = (index[i] < 0) ? R_NilValue : vec[ index[i] ] ;
        copy_most_attributes(out, vec) ;
        return out ;
    }
        
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
            
        inline SEXP subset( const Rcpp::IntegerVector& index) const {
            return promote( Parent::subset( index ) );
        }
        
        inline SEXP subset( const std::vector<int>& index) const {
            return promote( Parent::subset( index ) ) ;    
        }
        
        inline SEXP subset( const ChunkIndexMap& map ) const {
            return promote( Parent::subset( map ) ) ;
        }
        
        inline SEXP subset( const Rcpp::LogicalVector& index ) const {
            return promote( Parent::subset( index ) ) ;
        }
        
        inline SEXP subset( EmptySubset empty) const {
            return promote( Parent::subset(empty) ) ;    
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
        
        inline SEXP promote( IntegerVector x) const {
            copy_most_attributes(x, vec ) ;
            return x ;
        }
        
        CharacterVector levels ;
        SEXP* levels_ptr ;
        comparisons<STRSXP> string_compare ;
    } ;
    
}

#endif
