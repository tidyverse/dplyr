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
      CLASS* obj = static_cast<CLASS*>(this);
      return process_data<GroupedDataFrame>(gdf, obj).run();
    }

    virtual SEXP process(const RowwiseDataFrame& gdf) {
      CLASS* obj = static_cast<CLASS*>(this);
      return process_data<RowwiseDataFrame>(gdf, obj).run();
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
  class process_data {
  public:
    process_data(const Data& gdf, CLASS* chunk_source_) : git(gdf.group_begin()), ngroups(gdf.ngroups()), chunk_source(chunk_source_) {}

    SEXP run() {
      if (ngroups == 0)
        return process_empty();

      process_first();
      return process_rest();
    }

  private:
    SEXP process_empty() {
      LOG_VERBOSE << "no groups to process";
      return LogicalVector(0, NA_LOGICAL);
    }

    void process_first() {
      const RObject& first_result = fetch_chunk();

      LOG_VERBOSE << "instantiating delayed processor for type " << first_result.sexp_type();

      processor.reset(get_delayed_processor<CLASS>(first_result, ngroups));
      if (!processor)
        stop("expecting a single value");

      LOG_VERBOSE << "processing " << ngroups << " groups with " << processor->describe() << " processor";
    }

    SEXP process_rest() {
      for (int i = 1; i < ngroups; ++i) {
        const RObject& chunk = fetch_chunk();
        if (!processor->try_handle(chunk)) {
          LOG_VERBOSE << "not handled group " << i;

          if (processor->can_promote(chunk)) {
            LOG_VERBOSE << "promoting after group " << i;

            processor.reset(
              processor->promote(chunk)
            );
          } else {
            stop("can't promote group %d to %s", i, processor->describe());
          }
        }
      }

      Shield<SEXP> res(processor->get());
      return res;
    }

    RObject fetch_chunk() {
      const RObject& chunk = chunk_source->process_chunk(*git);
      ++git;
      return chunk;
    }

  private:
    typename Data::group_iterator git;
    const int ngroups;
    boost::scoped_ptr<IDelayedProcessor> processor;
    CLASS* chunk_source;
  };

  };
}
#endif
