#ifndef dplyr_tools_SlicingIndex_H
#define dplyr_tools_SlicingIndex_H

class SlicingIndex {
public:

    SlicingIndex(IntegerVector data_) : data(data_), group_index(-1) {}
    SlicingIndex(IntegerVector data_, int group_) : data(data_), group_index(group_) {}

    SlicingIndex(int start, int n) : data(0), group_index(-1) {
        if(n>0) {
            data = seq(start, start + n - 1 ) ;
        }
    }

    inline int size() const {
        return data.size() ;
    }

    inline int operator[](int i) const {
        return data[i] ;
    }

    inline int group() const { return group_index ; }

// private:
    IntegerVector data ;
    int group_index ;
} ;

#endif
