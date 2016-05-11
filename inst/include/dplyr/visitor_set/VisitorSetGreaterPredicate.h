#ifndef dplyr_VisitorSetGreaterPredicate_H
#define dplyr_VisitorSetGreaterPredicate_H

namespace dplyr{

    template <typename VisitorSet>
    class VisitorSetGreaterPredicate {
    public:
        VisitorSetGreaterPredicate( const VisitorSet& visitors_ ) : visitors(visitors_) {} ;
        inline bool operator()(int i, int j) const {
            return visitors.greater(i,j) ;
        }

    private:
        const VisitorSet& visitors ;
    } ;

}

#endif
