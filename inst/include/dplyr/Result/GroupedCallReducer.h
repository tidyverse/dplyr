#ifndef dplyr_GroupedCallReducer_H
#define dplyr_GroupedCallReducer_H
#include <dplyr/DataMask.h>

#include <boost/scoped_ptr.hpp>

#include <tools/all_na.h>

#include <dplyr/Result/Result.h>
#include <dplyr/Result/DelayedProcessor.h>

#include <dplyr/bad.h>
#include <dplyr/DataMask.h>

namespace dplyr {

template <typename Data>
class GroupedCallReducer  {
public:
  typedef typename Data::slicing_index Index ;

  GroupedCallReducer( SEXP expr_, SymbolString name_, DataMask<Data>& data_mask_) :
    expr(expr_),
    name(name_),
    data_mask(data_mask_)
  {}

  SEXP process(const Data& gdf) ;

  inline SEXP process_chunk(const Index& indices) {
    return data_mask.eval(expr, indices);
  }

  const SymbolString& get_name() const {
    return name;
  }

private:
  SEXP expr;
  const SymbolString name;
  DataMask<Data> data_mask;
};


template <typename Data>
class process_data {
public:
  process_data(const Data& gdf, GroupedCallReducer<Data>& chunk_source_) :
    git(gdf.group_begin()),
    ngroups(gdf.ngroups()),
    chunk_source(chunk_source_)
  {}

  SEXP run() {
    if (ngroups == 0) {
      LOG_INFO << "no groups to process";
      return get_processed_empty();
    }

    LOG_INFO << "processing groups";
    process_first();
    process_rest();
    return get_processed();
  }

private:
  void process_first() {
    RObject first_result = fetch_chunk();
    LOG_INFO << "instantiating delayed processor for type " << type2name(first_result)
             << " for column `" << chunk_source.get_name().get_utf8_cstring() << "`";

    processor.reset(get_delayed_processor< GroupedCallReducer<Data> >(first_result, ngroups, chunk_source.get_name()));
    LOG_VERBOSE << "processing " << ngroups << " groups with " << processor->describe() << " processor";
  }

  void process_rest() {
    for (int i = 1; i < ngroups; ++i) {
      const RObject& chunk = fetch_chunk();
      if (!try_handle_chunk(chunk)) {
        LOG_VERBOSE << "not handled group " << i;
        handle_chunk_with_promotion(chunk, i);
      }
    }
  }

  bool try_handle_chunk(const RObject& chunk) const {
    return processor->try_handle(chunk);
  }

  void handle_chunk_with_promotion(const RObject& chunk, const int i) {
    IDelayedProcessor* new_processor = processor->promote(chunk);
    if (!new_processor) {
      bad_col(chunk_source.get_name(), "can't promote group {group} to {type}",
        _["group"] = i, _["type"] =  processor->describe());
    }

    LOG_VERBOSE << "promoted group " << i;
    processor.reset(new_processor);
  }

  RObject fetch_chunk() {
    RObject chunk = chunk_source.process_chunk(*git);
    ++git;
    return chunk;
  }

  SEXP get_processed() const {
    return processor->get();
  }

  static SEXP get_processed_empty() {
    return LogicalVector(0, NA_LOGICAL);
  }

private:
  typename Data::group_iterator git;
  const int ngroups;
  boost::scoped_ptr<IDelayedProcessor> processor;
  GroupedCallReducer<Data>& chunk_source;
};

template <typename Data>
inline SEXP GroupedCallReducer<Data>::process(const Data& gdf) {
  return process_data<Data>(gdf, *this).run();
}

template <>
inline SEXP GroupedCallReducer<NaturalDataFrame>::process(const NaturalDataFrame& gdf) {
  return process_chunk(NaturalSlicingIndex(gdf.nrows())) ;
}





}

#endif
