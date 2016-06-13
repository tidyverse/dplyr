#ifndef dplyr_tools_FullDataFrame_H
#define dplyr_tools_FullDataFrame_H

namespace Rcpp {

    class FullDataFrame {
    public:
        FullDataFrame( const DataFrame& data_ ) : index(0, data_.nrows() ) {}

        const SlicingIndex& get_index() const {
            return index ;
        }

        inline int nrows() const {
            return index.size() ;
        }

    private:
        SlicingIndex index ;
    } ;

}
#endif
