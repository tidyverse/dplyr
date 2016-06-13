#ifndef dplyr_Result_CallbackProcessor_H
#define dplyr_Result_CallbackProcessor_H

namespace dplyr{

    // classes inherit from this template when they have a method with this signature
    // SEXP process_chunk( const SlicingIndex& indices)
    //
    // the first time process_chunk is called, CallbackProcessor finds the right type
    // for storing the results, and it creates a suitable DelayedProcessor
    // object which is then used to fill the vector
    //
    // DelayedReducer is an example on how CallbackReducer is used
    //
    // it is assumed that the SEXP comes from evaluating some R expression, so
    // it should be one of a integer vector of length one, a numeric vector of
    // length one or a character vector of length one
    template <typename CLASS>
    class CallbackProcessor : public Result {
    public:
        CallbackProcessor(){}

        virtual SEXP process( const GroupedDataFrame& gdf){
            return process_data<GroupedDataFrame>( gdf ) ;
        }

        virtual SEXP process( const RowwiseDataFrame& gdf){
            return process_data<RowwiseDataFrame>( gdf ) ;
        }

        virtual SEXP process( const Rcpp::FullDataFrame& df){
            CLASS* obj = static_cast<CLASS*>(this) ;
            return obj->process_chunk(df.get_index()) ;
        }

        virtual SEXP process( const SlicingIndex& index ){
            return R_NilValue ;
        }

    private:

        template <typename Data>
        SEXP process_data( const Data& gdf ){
            CLASS* obj = static_cast<CLASS*>( this ) ;
            typename Data::group_iterator git = gdf.group_begin() ;

            // the group index
            int i = 0 ;
            int ngroups = gdf.ngroups() ;
            // evaluate the expression within each group until we find something that is not NA
            RObject first_result(R_NilValue) ;
            for( ; i<ngroups; i++, ++git ){
                first_result = obj->process_chunk(*git) ;
                if( ! all_na(first_result) ) break ;
            }
            // all the groups evaluated to NA, so we send a logical vector NA
            // perhaps the type of the vector could depend on something, maybe later
            if( i == ngroups ){
                return LogicalVector(ngroups, NA_LOGICAL) ;
            }

            // otherwise, instantiate a DelayedProcessor based on the first non NA
            // result we get

            // get the appropriate Delayed Processor to handle it
            boost::scoped_ptr< DelayedProcessor_Base<CLASS> > processor(
                get_delayed_processor<CLASS>(i, first_result, ngroups )
            ) ;
            if(!processor)
                stop( "expecting a single value" );
            for( ; i<ngroups ; i++, ++git ){
                first_result = obj->process_chunk(*git) ;
                if( !processor->handled(i, first_result) ){
                    if( processor->can_promote(first_result) ){
                        processor.reset(
                            processor->promote(i, first_result)
                        ) ;
                    }
                }
            }

            Shield<SEXP> res( processor->get() ) ;
            return res ;
        }

    } ;

}
#endif
