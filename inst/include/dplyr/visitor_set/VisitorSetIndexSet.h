#ifndef dplyr_VisitorSetIndexSet_H
#define dplyr_VisitorSetIndexSet_H

namespace dplyr{
                  
    template <typename VisitorSet>
    class VisitorSetIndexSet : public boost::unordered_set<int, VisitorSetHasher<VisitorSet>, VisitorSetEqualPredicate<VisitorSet> > {
    private:
        typedef VisitorSetHasher<VisitorSet> Hasher ;
        typedef VisitorSetEqualPredicate<VisitorSet> EqualPredicate ;
        typedef boost::unordered_set<int, Hasher, EqualPredicate> Base ;
        
    public:
        VisitorSetIndexSet() : Base(){}
        
        VisitorSetIndexSet( VisitorSet& visitors_ ) : 
            Base( 1024, Hasher(&visitors_), EqualPredicate(&visitors_) )
        {}
        VisitorSetIndexSet( VisitorSet* visitors_ ) : 
            Base( 1024, Hasher(visitors_), EqualPredicate(visitors_) )
        {}
    } ;
    
}
#endif
