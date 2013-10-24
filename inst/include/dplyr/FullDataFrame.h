#ifndef dplyr_tools_FullDataFrame_H
#define dplyr_tools_FullDataFrame_H

namespace Rcpp {
    
    class FullDataFrame {
    public:
        FullDataFrame( const DataFrame& data_ ) : 
            index_data( seq(0, data_.nrows()-1 )), index(index_data) {}
         
        const Index_0_based& get_index() const { 
            return index ; 
        }    
            
    private:
        IntegerVector index_data ;
        Index_0_based index ;
    } ;
    
}
#endif              
