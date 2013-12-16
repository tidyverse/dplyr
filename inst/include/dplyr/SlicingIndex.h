#ifndef dplyr_tools_SlicingIndex_H
#define dplyr_tools_SlicingIndex_H

class SlicingIndex {
public:
    
    SlicingIndex(IntegerVector data_) : data(data_){}
    
    SlicingIndex(int start, int n) : data(seq(start,start+n-1)){}
    
    inline int size() const { 
        return data.size() ; 
    }
    
    inline int operator[](int i) const {
        return data[i] ;    
    }
    
private:
    IntegerVector data ;
} ;

#endif
