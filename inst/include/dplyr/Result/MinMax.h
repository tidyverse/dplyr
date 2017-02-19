#ifndef dplyr_Result_MinMax_H
#define dplyr_Result_MinMax_H

#include <dplyr/Result/is_smaller.h>
#include <dplyr/Result/Processor.h>

namespace dplyr {

  template <int RTYPE, bool MINIMUM, bool NA_RM>
  class MinMax : public Processor<RTYPE, MinMax<RTYPE, MINIMUM, NA_RM> > {

  public:
    typedef Processor<RTYPE, MinMax<RTYPE, MINIMUM, NA_RM> > Base;
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

  private:
    static const STORAGE Inf;

    class ChunkProcessor {
    public:
      ChunkProcessor(const STORAGE* data_ptr_, const SlicingIndex& indices_) :
        data_ptr(data_ptr_), indices(indices_), i(0), n(indices_.size()), res(Inf) {}

      STORAGE process() {
        process_loop<true>();
        process_loop<false>();
        return res;
      }

    private:
      template <bool ONLY_CHECK_NA>
      void process_loop() {
        for (; i < n; ++i) {
          STORAGE current = data_ptr[indices[i]];

          if (Rcpp::Vector<RTYPE>::is_na(current)) {
            if (NA_RM)
              continue;
            else {
              res = current;
              return;
            }
          }

          if (ONLY_CHECK_NA) {
            res = current;
            ++i;
            return;
          }
          else {
            if (is_better(current, res)) res = current;
          }
        }
      }

      inline bool is_better(const STORAGE& current, const STORAGE& res) {
        if (MINIMUM)
          return internal::is_smaller<RTYPE>(current, res);
        else
          return internal::is_smaller<RTYPE>(res, current);
      }

    private:
      const STORAGE* data_ptr;
      const SlicingIndex& indices;

      int i;
      int n;
      STORAGE res;
    };

  public:
    MinMax(SEXP x, bool is_summary_ = false) :
      Base(x),
      data_ptr(Rcpp::internal::r_vector_start<RTYPE>(x)),
      is_summary(is_summary_)
    {}
    ~MinMax() {}

    STORAGE process_chunk(const SlicingIndex& indices) {
      if (is_summary) return data_ptr[ indices.group() ];
      return ChunkProcessor(data_ptr, indices).process();
    }

  private:
    STORAGE* data_ptr;
    bool is_summary;
  };

  template <int RTYPE, bool MINIMUM, bool NA_RM>
  const typename MinMax<RTYPE, MINIMUM, NA_RM>::STORAGE MinMax<RTYPE, MINIMUM, NA_RM>::Inf =
    (RTYPE == REALSXP) ?
    (MINIMUM ? R_PosInf : R_NegInf) :
    NA_INTEGER;

}

#endif
