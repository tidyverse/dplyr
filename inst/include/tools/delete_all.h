#ifndef dplyr_tools_delete_all_H
#define dplyr_tools_delete_all_H

namespace dplyr {

    template <typename T>
    void delete_all_second( T& value ){
        for( typename T::iterator it=value.begin(); it!=value.end(); ++it) {
            delete it->second ;
        }
        value.clear() ;
    }

}

#endif
