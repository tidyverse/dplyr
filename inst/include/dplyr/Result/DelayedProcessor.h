#ifndef dplyr_Result_DelayedProcessor_H
#define dplyr_Result_DelayedProcessor_H

#include <tools/hash.h>
#include <tools/ShrinkableVector.h>
#include <tools/scalar_type.h>
#include <tools/utils.h>
#include <dplyr/vector_class.h>

namespace dplyr {

  class IDelayedProcessor {
  public:
    IDelayedProcessor() {}
    virtual ~IDelayedProcessor() {}

    virtual bool try_handle(const RObject& chunk) = 0;
    virtual bool can_promote(const RObject& chunk) = 0;
    virtual IDelayedProcessor* promote(const RObject& chunk) = 0;
    virtual SEXP get() = 0;
    virtual std::string describe() = 0;
  };

  template <int RTYPE>
  bool valid_conversion(int rtype) {
    return rtype == RTYPE;
  }

  template <>
  inline bool valid_conversion<REALSXP>(int rtype) {
    switch (rtype) {
    case REALSXP:
    case INTSXP:
    case LGLSXP:
      return true;
    default:
      break;
    }
    return false;
  }

  template <>
  inline bool valid_conversion<INTSXP>(int rtype) {
    switch (rtype) {
    case INTSXP:
    case LGLSXP:
      return true;
    default:
      break;
    }
    return false;
  }

  template <int RTYPE>
  inline bool valid_promotion(int rtype) {
    return false;
  }

  template <>
  inline bool valid_promotion<INTSXP>(int rtype) {
    return rtype == REALSXP;
  }

  template <>
  inline bool valid_promotion<LGLSXP>(int rtype) {
    return rtype == REALSXP || rtype == INTSXP;
  }

  template <int RTYPE, typename CLASS>
  class DelayedProcessor : public IDelayedProcessor {
  public:
    typedef typename traits::scalar_type<RTYPE>::type STORAGE;
    typedef Vector<RTYPE> Vec;

    DelayedProcessor(const RObject& first_result, int ngroups_) :
      res(no_init(ngroups_)), pos(0), seen_na_only(true)
    {
      if (!try_handle(first_result))
        stop("cannot handle result of type %i", first_result.sexp_type());
      copy_most_attributes(res, first_result);
    }

    DelayedProcessor(int pos_, const RObject& chunk, SEXP res_) :
      res(as<Vec>(res_)), pos(pos_), seen_na_only(false)
    {
      copy_most_attributes(res, chunk);
      if (!try_handle(chunk))
        stop("cannot handle result of type %i in promotion", chunk.sexp_type());
    }

    virtual bool try_handle(const RObject& chunk) {
      int rtype = TYPEOF(chunk);
      if (valid_conversion<RTYPE>(rtype)) {
        // copy, and memoize the copied value
        const typename Vec::stored_type& converted_chunk = (res[pos++] = as<STORAGE>(chunk));
        if (!Vec::is_na(converted_chunk))
          seen_na_only = false;
        return true;
      } else {
        return false;
      }
    }

    virtual bool can_promote(const RObject& chunk) {
      return seen_na_only || valid_promotion<RTYPE>(TYPEOF(chunk));
    }

    virtual IDelayedProcessor* promote(const RObject& chunk) {
      int rtype = TYPEOF(chunk);
      switch (rtype) {
      case LGLSXP:
        return new DelayedProcessor<LGLSXP , CLASS>(pos, chunk, res);
      case INTSXP:
        return new DelayedProcessor<INTSXP , CLASS>(pos, chunk, res);
      case REALSXP:
        return new DelayedProcessor<REALSXP, CLASS>(pos, chunk, res);
      case CPLXSXP:
        return new DelayedProcessor<CPLXSXP, CLASS>(pos, chunk, res);
      case STRSXP:
        return new DelayedProcessor<STRSXP, CLASS>(pos, chunk, res);
      default:
        break;
      }
      return 0;
    }

    virtual SEXP get() {
      return res;
    }

    virtual std::string describe() {
      return vector_class<RTYPE>();
    }


  private:
    Vec res;
    int pos;
    bool seen_na_only;

  };

  template <typename CLASS>
  class FactorDelayedProcessor : public IDelayedProcessor {
  private:
    typedef dplyr_hash_map<SEXP,int> LevelsMap;

  public:

    FactorDelayedProcessor(SEXP first_result, int ngroups) :
      res(ngroups, NA_INTEGER), pos(0)
    {
      copy_most_attributes(res, first_result);
      CharacterVector levels = Rf_getAttrib(first_result, Rf_install("levels"));
      int n = levels.size();
      for (int i=0; i<n; i++) levels_map[ levels[i] ] = i+1;
      if (!try_handle(first_result))
        stop("cannot handle factor result");
    }

    virtual bool try_handle(const RObject& chunk) {
      CharacterVector lev = chunk.attr("levels");
      update_levels(lev);

      int val = as<int>(chunk);
      if (val == NA_INTEGER) {
        return true;
      }
      SEXP s = lev[val-1];
      res[pos++] = levels_map[s];
      return true;
    }

    virtual bool can_promote(const RObject& chunk) {
      return false;
    }

    virtual IDelayedProcessor* promote(const RObject& chunk) {
      return 0;
    }

    virtual SEXP get() {
      int n = levels_map.size();
      CharacterVector levels(n);
      LevelsMap::iterator it = levels_map.begin();
      for (int i=0; i<n; i++, ++it) {
        levels[it->second-1] = it->first;
      }
      res.attr("class") = "factor";
      res.attr("levels") = levels;
      return res;
    }

    virtual std::string describe() {
      return "factor";
    }

  private:

    void update_levels(const CharacterVector& lev) {
      int nlevels = levels_map.size();
      int n = lev.size();
      for (int i=0; i<n; i++) {
        SEXP s = lev[i];
        if (! levels_map.count(s)) {
          levels_map.insert(std::make_pair(s, ++nlevels));
        }
      }
    }

    IntegerVector res;
    int pos;
    LevelsMap levels_map;
  };



  template <typename CLASS>
  class DelayedProcessor<VECSXP, CLASS> : public IDelayedProcessor {
  public:
    DelayedProcessor(SEXP first_result, int ngroups) :
      res(ngroups), pos(0)
    {
      copy_most_attributes(res, first_result);
      if (!try_handle(first_result))
        stop("cannot handle list result");
    }

    virtual bool try_handle(const RObject& chunk) {
      if (is<List>(chunk) && Rf_length(chunk) == 1) {
        res[pos++] = maybe_copy(VECTOR_ELT(chunk, 0));
        return true;
      }
      return false;
    }

    virtual bool can_promote(const RObject& chunk) {
      return false;
    }

    virtual IDelayedProcessor* promote(const RObject& chunk) {
      return 0;
    }

    virtual SEXP get() {
      return res;
    }

    virtual std::string describe() {
      return "list";
    }

  private:
    List res;
    int pos;

    inline SEXP maybe_copy(SEXP x) const {
      return is_ShrinkableVector(x) ? Rf_duplicate(x) : x;
    }
  };

  template <typename CLASS>
  IDelayedProcessor* get_delayed_processor(SEXP first_result, int ngroups) {
    if (Rf_length(first_result) != 1)
      stop("expecting a single value, got %d", Rf_length(first_result));

    if (Rf_inherits(first_result, "factor")) {
      return new FactorDelayedProcessor<CLASS>(first_result, ngroups);
    } else if (Rcpp::is<int>(first_result)) {
      return new DelayedProcessor<INTSXP, CLASS>(first_result, ngroups);
    } else if (Rcpp::is<double>(first_result)) {
      return new DelayedProcessor<REALSXP, CLASS>(first_result, ngroups);
    } else if (Rcpp::is<Rcpp::String>(first_result)) {
      return new DelayedProcessor<STRSXP, CLASS>(first_result, ngroups);
    } else if (Rcpp::is<bool>(first_result)) {
      return new DelayedProcessor<LGLSXP, CLASS>(first_result, ngroups);
    } else if (Rcpp::is<Rcpp::List>(first_result)) {
      return new DelayedProcessor<VECSXP, CLASS>(first_result, ngroups);
    } else if (TYPEOF(first_result) == CPLXSXP) {
      return new DelayedProcessor<CPLXSXP, CLASS>(first_result, ngroups);
    }

    stop("unknown result of type %d", TYPEOF(first_result));
  }

}
#endif
