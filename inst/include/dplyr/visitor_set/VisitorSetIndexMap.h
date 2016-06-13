#ifndef dplyr_VisitorSetIndexMap_H
#define dplyr_VisitorSetIndexMap_H

namespace dplyr{

    template <typename VisitorSet, typename VALUE>
    class VisitorSetIndexMap :
        public dplyr_hash_map<int, VALUE, VisitorSetHasher<VisitorSet> , VisitorSetEqualPredicate<VisitorSet> > {
    private:
        typedef VisitorSetHasher<VisitorSet> Hasher ;
        typedef VisitorSetEqualPredicate<VisitorSet> EqualPredicate ;
        typedef typename dplyr_hash_map<int, VALUE, Hasher, EqualPredicate> Base ;

    public:
        VisitorSetIndexMap() : Base(), visitors(0) {}

        VisitorSetIndexMap( VisitorSet& visitors_ ) :
            Base( 1024, Hasher(&visitors_), EqualPredicate(&visitors_) ),
            visitors(&visitors_)
        {}

        VisitorSetIndexMap( VisitorSet* visitors_ ) :
            Base( 1024, Hasher(visitors_), EqualPredicate(visitors_) ),
            visitors(visitors_)
        {}

        VisitorSet* visitors ;

    } ;

}

#endif
