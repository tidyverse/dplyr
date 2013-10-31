#ifndef dplyr_CountDistinct_H
#define dplyr_CountDistinct_H

namespace dplyr {

    template <typename Visitor>
    class CountDistinct{
    public:    
        CountDistinct( const Visitor& v_ ) : 
            v(v_), set( 1024, VisitorHash<Visitor>(v), VisitorEqualPredicate<Visitor>(v) )
        {
            int n = v.size() ;
            for( int i=0; i<n; i++)
                set.insert(i);
        }
        
        inline operator int() const{
            return set.size() ;    
        }
        
    private:
        const Visitor& v ;
        boost::unordered_set<int, VisitorHash<Visitor>, VisitorEqualPredicate<Visitor> > set ;
    } ;
    
}
#endif
