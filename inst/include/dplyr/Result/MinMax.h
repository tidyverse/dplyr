#ifndef dplyr_Result_MinMax_H
#define dplyr_Result_MinMax_H

#include <dplyr/Result/is_smaller.h>
#include <dplyr/Result/Processor.h>

namespace dplyr {

namespace detail {

template <int RTYPE, bool MINIMUM>
struct MinMaxDefault {
};

template <bool MINIMUM>
struct MinMaxDefault<INTSXP, MINIMUM> {
  static int get_worst() {
    if (MINIMUM)
      return INT_MAX;
    else
      return -INT_MAX;
  }

  static int get_default() {
    return NA_INTEGER;
  }
};

template <bool MINIMUM>
struct MinMaxDefault<REALSXP, MINIMUM> {
  static double get_worst() {
    if (MINIMUM)
      return R_PosInf;
    else
      return R_NegInf;
  }

  static double get_default() {
    return get_worst();
  }
};

}

template <int RTYPE, bool MINIMUM, bool NA_RM>
class MinMax : public Processor<RTYPE, MinMax<RTYPE, MINIMUM, NA_RM> > {

public:
  typedef Processor<RTYPE, MinMax<RTYPE, MINIMUM, NA_RM> > Base;
  typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;

private:
  class Aggregator {
  public:
    Aggregator(const STORAGE* data_ptr_, const SlicingIndex& indices_) :
      data_ptr(data_ptr_),
      indices(indices_),
      i(0),
      res(detail::MinMaxDefault<RTYPE, MINIMUM>::get_worst()) {}

  public:
    STORAGE run() {
      if (!fetch_next()) {
        return detail::MinMaxDefault<RTYPE, MINIMUM>::get_default();
      }

      while (fetch_next()) {}
      return res;
    }

  private:
    bool fetch_next() {
      for (; i < indices.size(); ++i) {
        STORAGE current = data_ptr[indices[i]];

        if (Rcpp::Vector<RTYPE>::is_na(current)) {
          if (NA_RM)
            continue;
          else {
            res = Rcpp::Vector<RTYPE>::get_na();
            i = indices.size();
            return true;
          }
        }

        if (is_better(current, res)) {
          res = current;
          return true;
        }
      }

      return false;
    }

  inline static bool is_better(const STORAGE current, const STORAGE res) {
    if (MINIMUM)
      return internal::is_smaller<RTYPE>(current, res);
    else
      return internal::is_smaller<RTYPE>(res, current);
  }

  private:
    const STORAGE* data_ptr;
    const SlicingIndex& indices;
    int i;
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

    return Aggregator(data_ptr, indices).run();
  }

private:
  const STORAGE* data_ptr;
  bool is_summary;
};

}

#endif
