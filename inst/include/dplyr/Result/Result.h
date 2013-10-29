#ifndef dplyr_Result_H
#define dplyr_Result_H

namespace dplyr {
    
    // we can either derive from Result directly and implement process
    // manually by traversing the ChunkIndexMap
    // or we can use the Processor template class
    class Result {
    public:
        Result(){}
        virtual ~Result(){} ;
        
        virtual SEXP process( const GroupedDataFrame& gdf) = 0 ;
        
        virtual SEXP process( const FullDataFrame& df ) = 0 ;
        
        virtual SEXP process( const SlicingIndex& index ){
            return R_NilValue ;    
        }
        
    } ;

} // namespace dplyr

#endif
