#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

#include <tools/utils.h>
#include <tools/match.h>

#include <dplyr/join_match.h>
#include <dplyr/JoinVisitor.h>

namespace dplyr {

  CharacterVector get_uniques(const CharacterVector& left, const CharacterVector& right);

  void check_attribute_compatibility(SEXP left, SEXP right);

  template <int LHS_RTYPE, int RHS_RTYPE>
  class DualVector {
    typedef Vector<LHS_RTYPE> LHS_Vec;
    typedef Vector<RHS_RTYPE> RHS_Vec;
    typedef typename Rcpp::traits::storage_type<LHS_RTYPE>::type LHS_STORAGE;
    typedef typename Rcpp::traits::storage_type<RHS_RTYPE>::type RHS_STORAGE;

  public:
    DualVector(LHS_Vec left_, RHS_Vec right_) : left(left_), right(right_) {
      check_attribute_compatibility(left, right);
    }

    LHS_STORAGE get_value_as_left(const int i) const {
      if (i >= 0) return get_left_value(i);
      else return Rcpp::internal::r_coerce<RHS_RTYPE, LHS_RTYPE>(get_right_value(i));
    }

    RHS_STORAGE get_value_as_right(const int i) const {
      if (i >= 0) return Rcpp::internal::r_coerce<LHS_RTYPE, RHS_RTYPE>(get_left_value(i));
      else return get_right_value(i);
    }

    LHS_STORAGE get_left_value(const int i) const {
      if (i < 0) stop("get_left_value() called with negative argument");
      return left[i];
    }

    RHS_STORAGE get_right_value(const int i) const {
      if (i >= 0) stop("get_right_value() called with nonnegative argument");
      return right[-i - 1];
    }

    template <class iterator>
    SEXP subset(iterator it, const int n) {
      // We use the fact that LGLSXP < INTSXP < REALSXP, this defines our coercion precedence
      if (LHS_RTYPE == RHS_RTYPE)
        return subset_same(it, n);
      else if (LHS_RTYPE > RHS_RTYPE)
        return subset_left(it, n);
      else
        return subset_right(it, n);
    }

    template <class iterator>
    SEXP subset_same(iterator it, const int n) {
      stop("NYI");
    }

    template <class iterator>
    SEXP subset_left(iterator it, const int n) {
      LHS_Vec res = no_init(n);
      for (int i=0; i<n; i++, ++it) {
        res[i] = get_value_as_left(*it);
      }
      return res;
    }

    template <class iterator>
    SEXP subset_right(iterator it, const int n) {
      RHS_Vec res = no_init(n);
      for (int i=0; i<n; i++, ++it) {
        res[i] = get_value_as_right(*it);
      }
      return res;
    }

    SEXP get_left() {
      return left;
    }

  private:
    LHS_Vec left;
    RHS_Vec right;
  };

  template <int LHS_RTYPE, int RHS_RTYPE, bool NA_MATCH = true>
  class JoinVisitorImpl : public JoinVisitor {
  protected:
    typedef Vector<LHS_RTYPE> LHS_Vec;
    typedef Vector<RHS_RTYPE> RHS_Vec;
    typedef boost::hash<int> hasher;

  public:
    JoinVisitorImpl(LHS_Vec left, RHS_Vec right) : dual(left, right) {}

    size_t hash(int i);

    inline bool equal(int i, int j) {
      if (i >= 0 && j >= 0) {
        return join_match<LHS_RTYPE, LHS_RTYPE, NA_MATCH>::is_match(dual.get_left_value(i), dual.get_left_value(j));
      } else if (i < 0 && j < 0) {
        return join_match<RHS_RTYPE, RHS_RTYPE, NA_MATCH>::is_match(dual.get_right_value(i), dual.get_right_value(j));
      } else if (i >= 0 && j < 0) {
        return join_match<LHS_RTYPE, RHS_RTYPE, NA_MATCH>::is_match(dual.get_left_value(i), dual.get_right_value(j));
      } else {
        return join_match<RHS_RTYPE, LHS_RTYPE, NA_MATCH>::is_match(dual.get_right_value(i), dual.get_left_value(j));
      }
    }

    SEXP subset(const std::vector<int>& indices) {
      return dual.subset(indices.begin(), indices.size());
    }

    SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return dual.subset(set.begin(), set.size());
    }

  public:
    hasher LHS_hash_fun;
    hasher RHS_hash_fun;

  private:
    DualVector<LHS_RTYPE, RHS_RTYPE> dual;
  };

  template <typename Visitor>
  class Subsetter {
    typedef typename Visitor::Vec Vec;

  public:
    Subsetter(const Visitor& v_) : v(v_) {};

    template<class iterator>
    inline SEXP subset(iterator begin, const int n) {
      Vec res = no_init(n);
      iterator it = begin;
      for (int i=0; i<n; i++, ++it) {
        res[i] = v.get(*it);
      }
      return res;
    }

  private:
    const Visitor& v;
  };

  template <int RTYPE, bool NA_MATCH>
  class JoinVisitorImpl<RTYPE, RTYPE, NA_MATCH> : public JoinVisitor {
    typedef join_match<RTYPE, RTYPE, NA_MATCH> match;

  public:
    typedef Vector<RTYPE> Vec;

  protected:
    typedef Vec LHS_Vec;
    typedef Vec RHS_Vec;

  public:
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
    typedef boost::hash<STORAGE> hasher;

    JoinVisitorImpl(Vec left, Vec right) : dual(left, right) {}

    inline size_t hash(int i) {
      STORAGE x = get(i);

      // If NAs don't match, we want to distribute their hashes as evenly as possible
      if (!NA_MATCH && Vec::is_na(x))
        return static_cast<size_t>(i);
      else
        return hash_fun(x);
    }

    inline bool equal(int i, int j) {
      return match::is_match(get(i), get(j));
    }

    template<class iterator>
    inline SEXP subset(iterator it, const int n) {
      RObject res = Subsetter<JoinVisitorImpl>(*this).subset(it, n);
      copy_most_attributes(res, dual.get_left());
      return res;
    }

    inline SEXP subset(const std::vector<int>& indices) {
      return subset(indices.begin(), indices.size());
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return subset(set.begin(), set.size());
    }

    inline STORAGE get(int i) const {
      return i >= 0 ? dual.get_left_value(i) : dual.get_right_value(i);
    }

  protected:
    DualVector<RTYPE, RTYPE> dual;
    hasher hash_fun;
  };

  template <bool NA_MATCH = true>
  class POSIXctJoinVisitor : public JoinVisitorImpl<REALSXP, REALSXP, NA_MATCH> {
    typedef JoinVisitorImpl<REALSXP, REALSXP, NA_MATCH> Parent;

  public:
    POSIXctJoinVisitor(NumericVector left, NumericVector right) :
      Parent(left, right),
      tzone(R_NilValue)
    {
      RObject tzone_left  = left.attr("tzone");
      RObject tzone_right = right.attr("tzone");
      if (tzone_left.isNULL() && tzone_right.isNULL()) return;

      if (tzone_left.isNULL()) {
        tzone = tzone_right;
      } else if (tzone_right.isNULL()) {
        tzone = tzone_left;
      } else {
        std::string s_left  = as<std::string>(tzone_left);
        std::string s_right = as<std::string>(tzone_right);

        if (s_left == s_right) {
          tzone = wrap(s_left);
        } else {
          tzone = wrap("UTC");
        }
      }
    }

    inline SEXP subset(const std::vector<int>& indices) {
      return promote(Parent::subset(indices));
    }
    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return promote(Parent::subset(set));
    }

  private:
    inline SEXP promote(NumericVector x) {
      set_class(x, Rcpp::CharacterVector::create("POSIXct", "POSIXt"));
      if (!tzone.isNULL()) {
        x.attr("tzone") = tzone;
      }
      return x;
    }

  private:
    RObject tzone;
  };

  class DateJoinVisitorGetter {
  public:
    virtual ~DateJoinVisitorGetter() {};
    virtual double get(int i) = 0;
    virtual bool is_na(int i) const = 0;
  };

  template <int LHS_RTYPE, int RHS_RTYPE, bool NA_MATCH = true>
  class DateJoinVisitor : public JoinVisitorImpl<LHS_RTYPE, RHS_RTYPE, NA_MATCH> {
    typedef JoinVisitorImpl<LHS_RTYPE, RHS_RTYPE, NA_MATCH> Parent;

  public:
    DateJoinVisitor(typename Parent::LHS_Vec left, typename Parent::RHS_Vec right) : Parent(left, right) {}

    inline SEXP subset(const std::vector<int>& indices) {
      return promote(Parent::subset(indices));
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return promote(Parent::subset(set));
    }

  private:
    static SEXP promote(SEXP x) {
      set_class(x, "Date");
      return x;
    }

  private:
    typename Parent::hasher hash_fun;
  };

  JoinVisitor* join_visitor(SEXP, SEXP, const std::string&, const std::string&, bool warn);

}

#endif
