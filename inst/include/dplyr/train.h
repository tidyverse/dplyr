#ifndef dplyr_train_h
#define dplyr_train_h
    
namespace dplyr {
    
    template <typename Map>
    inline void train_push_back( Map& map, int n){
        for( int i=0; i<n; i++) map[i].push_back(i) ;
    }
    template <typename Map>
    inline void train_push_back( Map& map, int n, int m){
        int i=0; 
        while(i < n){           
            for( int j=0; j<m || i <n ; j++, i++) map[i].push_back(i) ;
            R_CheckUserInterrupt() ;
        }
    }
    
    template <typename Set>
    inline void train_insert( Set& set, int n){
        for( int i=0; i<n; i++) set.insert(i) ;
    }
    template <typename Set>
    inline void train_insert_right( Set& set, int n){
        for( int i=0; i<n; i++) set.insert(-i-1) ;
    }

}
#endif
