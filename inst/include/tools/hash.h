#ifndef dplyr_HASH_H
#define dplyr_HASH_H

inline std::size_t hash_value(const Rcomplex& cx){
    boost::hash<double> hasher;
    size_t seed = hasher(cx.r) ;
    boost::hash_combine( seed, hasher(cx.i) ) ;
    return seed ;
}

#endif
