#ifndef dplyr_JoinColumnSuffixer_H
#define dplyr_JoinColumnSuffixer_H

namespace dplyr{
    
    class JoinColumnSuffixer {
    public:
        JoinColumnSuffixer( const CharacterVector& lhs, const CharacterVector& rhs, const CharacterVector& by){
            CharacterVector table = setdiff( intersect( lhs, rhs), by ) ;
            int n=table.size() ;
            for( int i=0; i<n; i++) set.insert( table[i] ) ;
        }
        
        String get( String s, const char* suffix ){
            if( set.count(s.get_sexp()) ) s += suffix ;
            return s ;
        }
        
    private:
        typedef dplyr_hash_set<SEXP> Set ;
        Set set ;
    } ;

}
#endif
