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
        
    private:
        Vector<RTYPE> left, right ;
        hasher hash_fun ;
        
        inline STORAGE get(int i){
            return i>=0 ? left[i] : right[-i-1] ;    
        }
        
    } ;
    
}

#endif

