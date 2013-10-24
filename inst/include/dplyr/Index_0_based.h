#ifndef dplyr_tools_Index_0_based_H
#define dplyr_tools_Index_0_based_H

class Index_0_based {
public:
    Index_0_based( SEXP data ) : p(INTEGER(data)), n(Rf_length(data)) {}
    
    inline int size() const { 
        return n ; 
    }
    
    inline int operator[](int i) const {
        return p[i];    
    }
    
private:
    int* p ;
    int n ;
} ;

#endif
