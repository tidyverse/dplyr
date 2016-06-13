#ifndef dplyr_get_all_second_H
#define dplyr_get_all_second_H

namespace dplyr {

    template <typename Map>
    List get_all_second( const Map& map){
        int ngroups = map.size() ;
        List res(ngroups);
        typename Map::const_iterator it=map.begin() ;
        for( int i=0; i<ngroups; i++, ++it)
            res[i] = it->second ;
        return res ;
    }

}

#endif
