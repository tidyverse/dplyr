#ifndef dplyr_DataFrameVisitors_map_H
#define dplyr_DataFrameVisitors_map_H

namespace dplyr {

    typedef VisitorSetIndexMap< DataFrameVisitors, std::vector<int> > ChunkIndexMap ;

}

#endif
