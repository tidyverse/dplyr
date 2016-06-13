#ifndef dplyr_Result_H
#define dplyr_Result_H

namespace dplyr {

    class Result {
    public:
        Result(){}

        virtual ~Result(){} ;

        virtual SEXP process( const RowwiseDataFrame& gdf) = 0 ;

        virtual SEXP process( const GroupedDataFrame& gdf) = 0 ;

        virtual SEXP process( const FullDataFrame& df ) = 0 ;

        virtual SEXP process( const SlicingIndex& index ){
            return R_NilValue ;
        }

    } ;

} // namespace dplyr

#endif
