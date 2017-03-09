#include <dplyr/main.h>

#include <tools/utils.h>

#include <dplyr/visitor_set/VisitorSetIndexSet.h>

#include <dplyr/JoinVisitorImpl.h>

namespace dplyr {

  inline bool is_bare_vector(SEXP x) {
    SEXP att = ATTRIB(x);

    // only allow R_Names. as in R's do_isvector
    while (att != R_NilValue) {
      SEXP tag = TAG(att);
      if (!(tag == R_NamesSymbol || tag == Rf_install("comment"))) return false;
      att = CDR(att);
    }

    return true;
  }


  // -------------- (int,lgl)
  template <>
  inline size_t JoinVisitorImpl<INTSXP, LGLSXP>::hash(int i) {
    return RHS_hash_fun(dual.get_value_as_left(i));
  }
  template <>
  inline size_t JoinVisitorImpl<LGLSXP,INTSXP>::hash(int i) {
    return RHS_hash_fun(dual.get_value_as_left(i));
  }


  // -------------- (int,double)
  template <>
  inline size_t JoinVisitorImpl<INTSXP, REALSXP>::hash(int i) {
    return RHS_hash_fun(dual.get_value_as_right(i));
  }
  template <>
  inline size_t JoinVisitorImpl<LGLSXP, REALSXP>::hash(int i) {
    return RHS_hash_fun(dual.get_value_as_right(i));
  }


  // -------------- (double,int)
  template <>
  inline size_t JoinVisitorImpl<REALSXP, INTSXP>::hash(int i) {
    return LHS_hash_fun(dual.get_value_as_left(i));
  }
  template <>
  inline size_t JoinVisitorImpl<REALSXP, LGLSXP>::hash(int i) {
    return LHS_hash_fun(dual.get_value_as_left(i));
  }


  int count_attributes(SEXP x) {
    int n = 0;

    while (! Rf_isNull(x)) {
      SEXP name = TAG(x);
      if (name != R_NamesSymbol && name != R_DimSymbol) n++;
      x = CDR(x);
    }

    return n;
  }

  SEXP grab_attribute(SEXP name, SEXP x) {
    while (!Rf_isNull(x)) {
      if (TAG(x) == name) return CAR(x);
      x = CDR(x);
    }
    stop("cannot find attribute '%s' ", CHAR(PRINTNAME(name)));
  }

  void check_attribute_compatibility(SEXP left, SEXP right) {
    SEXP att_left  = ATTRIB(left);
    SEXP att_right = ATTRIB(right);
    int n_left = count_attributes(att_left);
    int n_right = count_attributes(att_right);

    if (n_left != n_right)
      stop("attributes of different sizes");

    List list_left(n_left), list_right(n_left);

    SEXP p_left = att_left;
    int i = 0;
    while (!Rf_isNull(p_left)) {
      SEXP name = TAG(p_left);
      if (name != R_NamesSymbol && name != R_DimSymbol) {
        list_left[i]  = CAR(p_left);
        list_right[i] = grab_attribute(name, att_right);
      }
      p_left = CDR(p_left);
    }
    RObject test = Language("all.equal", list_left, list_right).fast_eval();
    if (!is<bool>(test) || !as<bool>(test)) {
      stop("attributes are different");
    }
  }

  CharacterVector reencode_factor(IntegerVector x);

  R_xlen_t get_first_reencode_pos(const CharacterVector& xc) {
    R_xlen_t len = xc.length();
    for (R_xlen_t i = 0; i < len; ++i) {
      SEXP xci = xc[i];
      if (xci != NA_STRING && !IS_ASCII(xci) && !IS_UTF8(xci)) {
        return i;
      }
    }

    return len;
  }

  CharacterVector reencode_char(SEXP x) {
    if (Rf_isFactor(x)) return reencode_factor(x);

    CharacterVector xc(x);
    R_xlen_t first = get_first_reencode_pos(xc);
    if (first >= xc.length()) return x;

    CharacterVector ret(Rf_duplicate(xc));

    R_xlen_t len = ret.length();
    for (R_xlen_t i = first; i < len; ++i) {
      SEXP reti = ret[i];
      if (reti != NA_STRING && !IS_ASCII(reti) && !IS_UTF8(reti)) {
        ret[i] = String(Rf_translateCharUTF8(reti), CE_UTF8);
      }
    }

    return ret;
  }

  CharacterVector reencode_factor(IntegerVector x) {
    CharacterVector levels(reencode_char(get_levels(x)));
    CharacterVector ret(x.length());

    R_xlen_t nlevels = levels.length();

    R_xlen_t len = x.length();
    for (R_xlen_t i = 0; i < len; ++i) {
      int xi = x[i];
      if (xi <= 0 || xi > nlevels)
        ret[i] = NA_STRING;
      else
        ret[i] = levels[xi - 1];
    }

    return ret;
  }

  template <int LHS_RTYPE>
  JoinVisitor* date_join_visitor_right(SEXP left, SEXP right) {
    switch (TYPEOF(right)) {
    case INTSXP:
      return new DateJoinVisitor<LHS_RTYPE, INTSXP>(left, right);
    case REALSXP:
      return new DateJoinVisitor<LHS_RTYPE, REALSXP>(left, right);
    default:
      stop("Date objects should be represented as integer or numeric");
    }
  }

  JoinVisitor* date_join_visitor(SEXP left, SEXP right) {
    switch (TYPEOF(left)) {
    case INTSXP:
      return date_join_visitor_right<INTSXP>(left, right);
    case REALSXP:
      return date_join_visitor_right<REALSXP>(left, right);
    default:
      stop("Date objects should be represented as integer or numeric");
    }
  }

  JoinVisitor* join_visitor(SEXP left, SEXP right, const std::string& name_left, const std::string& name_right, bool warn_) {
    // handle Date separately
    bool lhs_date = Rf_inherits(left, "Date");
    bool rhs_date = Rf_inherits(right, "Date");

    switch (lhs_date + rhs_date) {
    case 2:
      return date_join_visitor(left, right);
    case 1:
      stop("cannot join a Date object with an object that is not a Date object");
    case 0:
      break;
    default:
      break;
    }

    bool lhs_time = Rf_inherits(left, "POSIXct");
    bool rhs_time = Rf_inherits(right, "POSIXct");
    switch (lhs_time + rhs_time) {
    case 2:
      return new POSIXctJoinVisitor<>(left, right);
    case 1:
      stop("cannot join a POSIXct object with an object that is not a POSIXct object");
    case 0:
      break;
    default:
      break;
    }

    switch (TYPEOF(left)) {
    case CPLXSXP:
    {
      switch (TYPEOF(right)) {
      case CPLXSXP:
        return new JoinVisitorImpl<CPLXSXP, CPLXSXP>(left, right);
      default:
        break;
      }
      break;
    }
    case INTSXP:
    {
      bool lhs_factor = Rf_inherits(left, "factor");
      switch (TYPEOF(right)) {
      case INTSXP:
      {
        bool rhs_factor = Rf_inherits(right, "factor");
        if (lhs_factor && rhs_factor) {
          if (same_levels(left, right)) {
            return new JoinVisitorImpl<INTSXP, INTSXP>(left, right);
          } else {
            if (warn_) Rf_warning("joining factors with different levels, coercing to character vector");
            return new JoinVisitorImpl<STRSXP, STRSXP>(reencode_char(left), reencode_char(right));
          }
        } else if (!lhs_factor && !rhs_factor) {
          return new JoinVisitorImpl<INTSXP, INTSXP>(left, right);
        }
        break;
      }
      case REALSXP:
      {
        if (!lhs_factor && is_bare_vector(right)) {
          return new JoinVisitorImpl<INTSXP, REALSXP>(left, right);
        }
        break;
        // what else: perhaps we can have INTSXP which is a Date and REALSXP which is a Date too ?
      }
      case LGLSXP:
      {
        if (!lhs_factor) {
          return new JoinVisitorImpl<INTSXP, LGLSXP>(left, right);
        }
        break;
      }
      case STRSXP:
      {
        if (lhs_factor) {
          if (warn_) Rf_warning("joining factor and character vector, coercing into character vector");
          return new JoinVisitorImpl<STRSXP, STRSXP>(reencode_char(left), reencode_char(right));
        }
      }
      default:
        break;
      }
      break;
    }
    case REALSXP:
    {
      switch (TYPEOF(right)) {
      case REALSXP:
        return new JoinVisitorImpl<REALSXP, REALSXP>(left, right);
      case INTSXP:
        return new JoinVisitorImpl<REALSXP, INTSXP>(left, right);
      default:
        break;
      }

    }
    case LGLSXP:
    {
      switch (TYPEOF(right)) {
      case LGLSXP:
        return new JoinVisitorImpl<LGLSXP,LGLSXP> (left, right);
      case INTSXP:
        return new JoinVisitorImpl<LGLSXP,INTSXP>(left, right);
      case REALSXP:
        return new JoinVisitorImpl<LGLSXP,REALSXP>(left, right);
      default:
        break;
      }
      break;
    }
    case STRSXP:
    {
      switch (TYPEOF(right)) {
      case INTSXP:
      {
        if (Rf_inherits(right, "factor")) {
          if (warn_) Rf_warning("joining character vector and factor, coercing into character vector");
          return new JoinVisitorImpl<STRSXP, STRSXP>(reencode_char(left), reencode_char(right));
        }
        break;
      }
      case STRSXP:
      {
        return new JoinVisitorImpl<STRSXP, STRSXP>(reencode_char(left), reencode_char(right));
      }
      default:
        break;
      }
      break;
    }
    default:
      break;
    }

    stop(
      "Can't join on '%s' x '%s' because of incompatible types (%s / %s)",
      name_left, name_right, get_single_class(left), get_single_class(right)
    );
    return 0;
  }

}
