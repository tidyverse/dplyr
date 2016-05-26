#ifndef dplyr_VisitorSetLessPredicate_H
#define dplyr_VisitorSetLessPredicate_H

namespace dplyr{

    template <typename VisitorSet>
    class VisitorSetLessPredicate {
    public:
        VisitorSetLessPredicate( const VisitorSet& visitors_ ) : visitors(visitors_) {} ;
        inline bool operator()(int i, int j) const {
            return visitors.less(i,j) ;
        }

    private:
        const VisitorSet& visitors ;
    } ;

}

#endif
