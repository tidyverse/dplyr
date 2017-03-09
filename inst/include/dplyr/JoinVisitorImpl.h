#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

#include <tools/utils.h>
#include <tools/match.h>

#include <dplyr/join_match.h>
#include <dplyr/JoinVisitor.h>

namespace dplyr {

  CharacterVector get_uniques(const CharacterVector& left, const CharacterVector& right);

  void check_attribute_compatibility(SEXP left, SEXP right);

  template <int LHS_RTYPE, int RHS_RTYPE, bool NA_MATCH = true>
  class JoinVisitorImpl : public JoinVisitor {
  public:
    typedef Vector<LHS_RTYPE> LHS_Vec;
    typedef Vector<RHS_RTYPE> RHS_Vec;

    typedef typename Rcpp::traits::storage_type<LHS_RTYPE>::type LHS_STORAGE;
    typedef typename Rcpp::traits::storage_type<RHS_RTYPE>::type RHS_STORAGE;

    typedef boost::hash<LHS_STORAGE> LHS_hasher;
    typedef boost::hash<RHS_STORAGE> RHS_hasher;

    JoinVisitorImpl(LHS_Vec left_, RHS_Vec right_) : left(left_), right(right_) {
      check_attribute_compatibility(left, right);
    }

    size_t hash(int i);

    inline bool equal(int i, int j) {
      if (i>=0 && j>=0) {
        return join_match<LHS_RTYPE, LHS_RTYPE, NA_MATCH>::is_match(left[i], left[j]);
      } else if (i < 0 && j < 0) {
        return join_match<RHS_RTYPE, RHS_RTYPE, NA_MATCH>::is_match(right[-i-1], right[-j-1]);
      } else if (i >= 0 && j < 0) {
        return join_match<LHS_RTYPE, RHS_RTYPE, NA_MATCH>::is_match(left[i], right[-j-1]);
      } else {
        return join_match<RHS_RTYPE, LHS_RTYPE, NA_MATCH>::is_match(right[-i-1], left[j]);
      }
    }

    template<class iterator>
    SEXP subset(iterator it, const int n);

    SEXP subset(const std::vector<int>& indices) {
      return subset(indices.begin(), indices.size());
    }

    SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return subset(set.begin(), set.size());
    }

    LHS_Vec left;
    RHS_Vec right;
    LHS_hasher LHS_hash_fun;
    RHS_hasher RHS_hash_fun;

  };

  template <typename Visitor>
  class Subsetter {
  public:
    typedef typename Visitor::Vec Vec;

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
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
    typedef boost::hash<STORAGE> hasher;

    JoinVisitorImpl(Vec left_, Vec right_) : left(left_), right(right_) {}

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
      copy_most_attributes(res, left);
      return res;
    }

    inline SEXP subset(const std::vector<int>& indices) {
      return subset(indices.begin(), indices.size());
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return subset(set.begin(), set.size());
    }

    inline STORAGE get(int i) const {
      return i >= 0 ? left[i] : right[-i-1];
    }

  protected:
    Vec left, right;
    hasher hash_fun;

  };

  template <bool NA_MATCH = true>
  class POSIXctJoinVisitor : public JoinVisitorImpl<REALSXP, REALSXP, NA_MATCH> {
  public:
    typedef JoinVisitorImpl<REALSXP, REALSXP, NA_MATCH> Parent;
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
    RObject tzone;

    inline SEXP promote(NumericVector x) {
      set_class(x, Rcpp::CharacterVector::create("POSIXct", "POSIXt"));
      if (!tzone.isNULL()) {
        x.attr("tzone") = tzone;
      }
      return x;
    }

  };

  class DateJoinVisitorGetter {
  public:
    virtual ~DateJoinVisitorGetter() {};
    virtual double get(int i) = 0;
    virtual bool is_na(int i) const = 0;
  };

  template <int RTYPE>
  class DateJoinVisitorGetterImpl : public DateJoinVisitorGetter {
  public:
    DateJoinVisitorGetterImpl(SEXP x) : data(x) {}

    inline double get(int i) {
      return static_cast<double>(data[i]);
    }

    inline bool is_na(int i) const {
      return data.is_na(data[i]);
    }

  private:
    Vector<RTYPE> data;
  };

  template <bool NA_MATCH = true>
  class DateJoinVisitor : public JoinVisitor {
    typedef join_match<REALSXP, REALSXP, NA_MATCH> match;

  public:
    typedef NumericVector Vec;
    typedef boost::hash<double> hasher;

    DateJoinVisitor(SEXP lhs, SEXP rhs)
    {
      if (TYPEOF(lhs) == INTSXP) {
        left = new DateJoinVisitorGetterImpl<INTSXP>(lhs);
      } else if (TYPEOF(lhs) == REALSXP) {
        left = new DateJoinVisitorGetterImpl<REALSXP>(lhs);
      } else {
        stop("Date objects should be represented as integer or numeric");
      }

      if (TYPEOF(rhs) == INTSXP) {
        right = new DateJoinVisitorGetterImpl<INTSXP>(rhs);
      } else if (TYPEOF(rhs) == REALSXP) {
        right = new DateJoinVisitorGetterImpl<REALSXP>(rhs);
      } else {
        stop("Date objects should be represented as integer or numeric");
      }

    }

    ~DateJoinVisitor() {
      delete left;
      delete right;
    }

    inline size_t hash(int i) {
      // If NAs don't match, we want to distribute their hashes as evenly as possible
      if (!NA_MATCH && is_na(i))
        return static_cast<size_t>(i);
      else
        return hash_fun(get(i));
    }
    inline bool equal(int i, int j) {
      return match::is_match(get(i), get(j));
    }

    template<class iterator>
    inline SEXP subset(iterator it, const int n) {
      NumericVector res = Subsetter<DateJoinVisitor>(*this).subset(it, n);
      set_class(res, "Date");
      return res;
    }

    inline SEXP subset(const std::vector<int>& indices) {
      return subset(indices.begin(), indices.size());
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      return subset(set.begin(), set.size());
    }

    inline double get(int i) const {
      if (i >= 0) {
        return left->get(i);
      } else {
        return right->get(-i-1);
      }
    }

    inline bool is_na(int i) const {
      if (i >= 0) {
        return left->is_na(i);
      } else {
        return right->is_na(-i-1);
      }
    }

  private:
    DateJoinVisitorGetter* left;
    DateJoinVisitorGetter* right;
    hasher hash_fun;

    DateJoinVisitor(const DateJoinVisitor&);
  };

  JoinVisitor* join_visitor(SEXP, SEXP, const std::string&, const std::string&, bool warn);

}

#endif
