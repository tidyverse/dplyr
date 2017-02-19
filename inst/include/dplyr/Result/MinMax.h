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

  public:
    MinMax(SEXP x, bool is_summary_ = false) :
      Base(x),
      data_ptr(Rcpp::internal::r_vector_start<RTYPE>(x)),
      is_summary(is_summary_)
    {}
    ~MinMax() {}

    STORAGE process_chunk(const SlicingIndex& indices) {
      if (is_summary) return data_ptr[ indices.group() ];

      const int n = indices.size();
      STORAGE res = Inf;

      bool first = true;
      for (int i = 0; i < n; ++i) {
        STORAGE current = data_ptr[indices[i]];

        if (Rcpp::Vector<RTYPE>::is_na(current)) {
          if (NA_RM)
            continue;
          else
            return current;
        }
        else {
          // Need a flag here, because is_better() cannot compare with Inf for RTYPE == INTSXP.
          // Hoping that compiler is able to split this in two loops, or that branch prediction
          // will make this check cheap.
          if (first) {
            res = current;
            first = false;
          }
          else {
            if (is_better(current, res))
              res = current;
          }
        }
      }

      return res;
    }

    inline static bool is_better(const STORAGE current, const STORAGE res) {
      if (MINIMUM)
        return internal::is_smaller<RTYPE>(current, res);
      else
        return internal::is_smaller<RTYPE>(res, current);
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
