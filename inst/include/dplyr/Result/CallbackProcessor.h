#ifndef dplyr_Result_CallbackProcessor_H
#define dplyr_Result_CallbackProcessor_H

#include <boost/scoped_ptr.hpp>

#include <tools/all_na.h>

#include <dplyr/Result/Result.h>
#include <dplyr/Result/DelayedProcessor.h>

namespace dplyr {

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
    CallbackProcessor() {}

    virtual SEXP process(const GroupedDataFrame& gdf) {
      return process_data<GroupedDataFrame>(gdf);
    }

    virtual SEXP process(const RowwiseDataFrame& gdf) {
      return process_data<RowwiseDataFrame>(gdf);
    }

    virtual SEXP process(const Rcpp::FullDataFrame& df) {
      CLASS* obj = static_cast<CLASS*>(this);
      return obj->process_chunk(df.get_index());
    }

    virtual SEXP process(const SlicingIndex& index) {
      return R_NilValue;
    }

  private:

    template <typename Data>
    SEXP process_data(const Data& gdf) {
      int ngroups = gdf.ngroups();

      if (ngroups == 0) {
        LOG_VERBOSE << "no groups to process";
        return LogicalVector(0, NA_LOGICAL);
      }

      CLASS* obj = static_cast<CLASS*>(this);
      typename Data::group_iterator git = gdf.group_begin();
      RObject first_result = obj->process_chunk(*git);

      LOG_VERBOSE << "instantiating delayed processor for type " << first_result.sexp_type();

      boost::scoped_ptr< DelayedProcessor_Base<CLASS> > processor(
        get_delayed_processor<CLASS>(0, first_result, ngroups)
      );
      if (!processor)
        stop("expecting a single value");

      LOG_VERBOSE << "processing " << ngroups << " groups";

      for (int i = 0; i < ngroups; ++i, ++git) {
        first_result = obj->process_chunk(*git);
        if (!processor->handled(i, first_result)) {
          LOG_VERBOSE << "not handled group " << i;

          if (processor->can_promote(first_result)) {
            LOG_VERBOSE << "promoting after group " << i;

            processor.reset(
              processor->promote(i, first_result)
            );
          } else {
            stop("can't promote group %d", i);
          }
        }
      }

      Shield<SEXP> res(processor->get());
      return res;
    }

  };

}
#endif
