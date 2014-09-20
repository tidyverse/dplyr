#ifndef dplyr_tools_OneBasedIndex_H
#define dplyr_tools_OneBasedIndex_H

template <class Source>
class OneBasedIndex {
public:
    
    OneBasedIndex(const Source& data_) : data(data_){}
    
    inline int size() const { 
        return data.size() ; 
    }
    
    inline int operator[](int i) const {
        return data[i] - 1 ;    
    }
    
    const Source& data ;
    int group_index ;
} ;

#endif
