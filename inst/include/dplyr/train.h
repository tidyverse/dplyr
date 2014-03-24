#ifndef dplyr_train_h
#define dplyr_train_h
    
namespace dplyr {
    
    template <typename Map>
    inline void train_push_back( Map& map, int n){
        for( int i=0; i<n; i++) map[i].push_back(i) ;
    }
    template <typename Map>
    inline void train_push_back( Map& map, int n, int m){
        if( m == 0 ) {
            train_push_back( map, n ) ;    
        } else {
            int ntrips = n / m ;
            int i=0; 
            for( int k=0; k<ntrips; k++){
                for( int j=0; j<m; j++, i++) map[i].push_back(i) ;
                Rcpp::checkUserInterrupt() ;
            }
            for( ; i<n; i++) map[i].push_back(i) ;
        }
    }
    
    template <typename Map>
    inline void train_push_back_right( Map& map, int n){
        for( int i=0; i<n; i++) map[-i-1].push_back(-i-1) ;
    }
    template <typename Map>
    inline void train_push_back_right( Map& map, int n, int m){
        if( m == 0 ){
            train_push_back_right(map, n) ;
        } else {
            int ntrips = n / m ;
            int i=0; 
            for( int k=0; k<ntrips; k++){
                for( int j=0; j<m; j++, i++) map[-i-1].push_back(-i-1) ;
                Rcpp::checkUserInterrupt() ;
            }
            for( ; i<n; i++) map[-i-1].push_back(-i-1) ;
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
