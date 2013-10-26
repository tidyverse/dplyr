#ifndef dplyr_tools_SlicingIndex_H
#define dplyr_tools_SlicingIndex_H

class SlicingIndex {
public:
    SlicingIndex( int start_, int n_) : start(start_), n(n_) {}
    
    inline int size() const { 
        return n ; 
    }
    
    inline int operator[](int i) const {
        return start + i ;    
    }
    
private:
    int start, n ;
} ;

#endif
