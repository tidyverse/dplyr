#ifndef dplyr_VisitorSetHash_H
#define dplyr_VisitorSetHash_H

namespace dplyr{
           
template <typename Class>
class VisitorSetHash {
public:
    size_t hash( int j) const {
        const Class& obj = static_cast<const Class&>(*this) ; 
        size_t seed = 0 ;
        int n = obj.size() ;
        for( int k=0; k<n; k++){
            boost::hash_combine( seed, obj.get(k)->hash(j) ) ;
        }
        return seed ;
    }
} ;



}

#endif
