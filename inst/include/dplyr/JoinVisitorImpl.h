#ifndef dplyr_JoinVisitorImpl_H
#define dplyr_JoinVisitorImpl_H

#include <tools/utils.h>
#include <tools/match.h>

#include <dplyr/comparisons.h>
#include <dplyr/comparisons_different.h>
#include <dplyr/JoinVisitor.h>

namespace dplyr {

  CharacterVector get_uniques(const CharacterVector& left, const CharacterVector& right);

  void check_attribute_compatibility(SEXP left, SEXP right);

  template <int LHS_RTYPE, int RHS_RTYPE>
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
        return comparisons<LHS_RTYPE>().equal_or_both_na(left[i], left[j]);
      } else if (i < 0 && j < 0) {
        return comparisons<RHS_RTYPE>().equal_or_both_na(right[-i-1], right[-j-1]);
      } else if (i >= 0 && j < 0) {
        return comparisons_different<LHS_RTYPE,RHS_RTYPE>().equal_or_both_na(left[i], right[-j-1]);
      } else {
        return comparisons_different<RHS_RTYPE,LHS_RTYPE>().equal_or_both_na(right[-i-1], left[j]);
      }
    }

    inline SEXP subset(const std::vector<int>& indices);
    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set);

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

    inline SEXP subset(const std::vector<int>& indices) {
      int n = indices.size();
      Vec res = no_init(n);
      for (int i=0; i<n; i++) {
        res[i] = v.get(indices[i]);
      }
      return res;
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      int n = set.size();
      Vec res = no_init(n);
      VisitorSetIndexSet<DataFrameJoinVisitors>::const_iterator it=set.begin();
      for (int i=0; i<n; i++, ++it) {
        res[i] = v.get(*it);
      }
      return res;
    }
  private:
    const Visitor& v;
  };

  template <int RTYPE>
  class JoinVisitorImpl<RTYPE,RTYPE> : public JoinVisitor {
  public:
    typedef Vector<RTYPE> Vec;
    typedef typename Rcpp::traits::storage_type<RTYPE>::type STORAGE;
    typedef boost::hash<STORAGE> hasher;

    JoinVisitorImpl(Vec left_, Vec right_) : left(left_), right(right_) {}

    inline size_t hash(int i) {
      return hash_fun(get(i));
    }

    inline bool equal(int i, int j) {
      return comparisons<RTYPE>().equal_or_both_na(get(i), get(j));
    }

    inline SEXP subset(const std::vector<int>& indices) {
      RObject res = Subsetter<JoinVisitorImpl>(*this).subset(indices);
      copy_most_attributes(res, left);
      return res;
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      RObject res = Subsetter<JoinVisitorImpl>(*this).subset(set);
      copy_most_attributes(res, left);
      return res;
    }

    inline STORAGE get(int i) const {
      return i >= 0 ? left[i] : right[-i-1];
    }

  protected:
    Vec left, right;
    hasher hash_fun;

  };

  class POSIXctJoinVisitor : public JoinVisitorImpl<REALSXP,REALSXP> {
  public:
    typedef JoinVisitorImpl<REALSXP,REALSXP> Parent;
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
  };

  template <int RTYPE>
  class DateJoinVisitorGetterImpl : public DateJoinVisitorGetter {
  public:
    DateJoinVisitorGetterImpl(SEXP x) : data(x) {}

    inline double get(int i) {
      return (double) data[i];
    }

  private:
    Vector<RTYPE> data;
  };

  class DateJoinVisitor : public JoinVisitor {
  public:
    typedef NumericVector Vec;
    typedef comparisons<REALSXP> Compare;
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
      return hash_fun(get(i));
    }
    inline bool equal(int i, int j) {
      return comparisons<REALSXP>().equal_or_both_na(get(i), get(j));
    }

    inline SEXP subset(const std::vector<int>& indices) {
      NumericVector res = Subsetter<DateJoinVisitor>(*this).subset(indices);
      set_class(res, "Date");
      return res;
    }

    inline SEXP subset(const VisitorSetIndexSet<DataFrameJoinVisitors>& set) {
      NumericVector res = Subsetter<DateJoinVisitor>(*this).subset(set);
      set_class(res, "Date");
      return res;
    }

    inline double get(int i) const {
      if (i>= 0) {
        return left->get(i);
      } else {
        return right->get(-i-1);
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
