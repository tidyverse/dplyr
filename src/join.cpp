#include "pch.h"
#include <dplyr/main.h>

#include <tools/encoding.h>
#include <tools/SymbolString.h>

#include <dplyr/visitors/join/JoinVisitorImpl.h>
#include <dplyr/symbols.h>

namespace dplyr {

inline bool is_bare_vector(SEXP x) {
  SEXP att = ATTRIB(x);

  // only allow R_Names. as in R's do_isvector
  while (att != R_NilValue) {
    SEXP tag = TAG(att);
    if (!(tag == R_NamesSymbol || tag == symbols::comment)) return false;
    att = CDR(att);
  }

  return true;
}

void warn_bad_var(const SymbolString& var_left, const SymbolString& var_right,
                  std::string message, bool warn = true) {
  if (!warn)
    return;

  if (var_left == var_right) {
    std::string var_utf8 = var_left.get_utf8_cstring();
    Rf_warningcall(
      R_NilValue,
      "Column `%s` %s",
      var_utf8.c_str(),
      message.c_str()
    );
  } else {
    std::string left_utf8 = var_left.get_utf8_cstring();
    std::string right_utf8 = var_right.get_utf8_cstring();
    Rf_warningcall(
      R_NilValue,
      "Column `%s`/`%s` %s",
      left_utf8.c_str(),
      right_utf8.c_str(),
      message.c_str()
    );
  }

}

void check_attribute_compatibility(const Column& left, const Column& right) {
  // Rely on R function based on all.equal
  static Rcpp::Function attr_equal = Rcpp::Function("attr_equal", dplyr::envs::ns_dplyr);
  Rcpp::Shield<SEXP> s_ok(attr_equal(left.get_data(), right.get_data()));
  if (!Rcpp::as<bool>(s_ok)) {
    warn_bad_var(left.get_name(), right.get_name(), "has different attributes on LHS and RHS of join");
  }
}

template <int LHS_RTYPE, bool ACCEPT_NA_MATCH>
JoinVisitor* date_join_visitor_right(const Column& left, const Column& right) {
  switch (TYPEOF(right.get_data())) {
  case INTSXP:
    return new DateJoinVisitor<LHS_RTYPE, INTSXP, ACCEPT_NA_MATCH>(left, right);
  case REALSXP:
    return new DateJoinVisitor<LHS_RTYPE, REALSXP, ACCEPT_NA_MATCH>(left, right);
  default:
    Rcpp::stop("Date objects should be represented as integer or numeric");
  }
}

template <bool ACCEPT_NA_MATCH>
JoinVisitor* date_join_visitor(const Column& left, const Column& right) {
  switch (TYPEOF(left.get_data())) {
  case INTSXP:
    return date_join_visitor_right<INTSXP, ACCEPT_NA_MATCH>(left, right);
  case REALSXP:
    return date_join_visitor_right<REALSXP, ACCEPT_NA_MATCH>(left, right);
  default:
    Rcpp::stop("Date objects should be represented as integer or numeric");
  }
}

template <bool ACCEPT_NA_MATCH>
JoinVisitor* join_visitor(const Column& left, const Column& right, bool warn_) {
  // handle Date separately
  bool lhs_date = Rf_inherits(left.get_data(), "Date");
  bool rhs_date = Rf_inherits(right.get_data(), "Date");

  switch (lhs_date + rhs_date) {
  case 2:
    return date_join_visitor<ACCEPT_NA_MATCH>(left, right);
  case 1:
    Rcpp::stop("cannot join a Date object with an object that is not a Date object");
  case 0:
    break;
  default:
    break;
  }

  bool lhs_time = Rf_inherits(left.get_data(), "POSIXct");
  bool rhs_time = Rf_inherits(right.get_data(), "POSIXct");
  switch (lhs_time + rhs_time) {
  case 2:
    return new POSIXctJoinVisitor<ACCEPT_NA_MATCH>(left, right);
  case 1:
    Rcpp::stop("cannot join a POSIXct object with an object that is not a POSIXct object");
  case 0:
    break;
  default:
    break;
  }

  switch (TYPEOF(left.get_data())) {
  case CPLXSXP:
  {
    switch (TYPEOF(right.get_data())) {
    case CPLXSXP:
      return new JoinVisitorImpl<CPLXSXP, CPLXSXP, ACCEPT_NA_MATCH>(left, right, warn_);
    default:
      break;
    }
    break;
  }
  case INTSXP:
  {
    bool lhs_factor = Rf_inherits(left.get_data(), "factor");
    switch (TYPEOF(right.get_data())) {
    case INTSXP:
    {
      bool rhs_factor = Rf_inherits(right.get_data(), "factor");
      if (lhs_factor && rhs_factor) {
        if (same_levels(left.get_data(), right.get_data())) {
          return new JoinVisitorImpl<INTSXP, INTSXP, ACCEPT_NA_MATCH>(left, right, warn_);
        } else {
          warn_bad_var(
            left.get_name(), right.get_name(),
            "joining factors with different levels, coercing to character vector",
            warn_
          );
          return
            new JoinVisitorImpl<STRSXP, STRSXP, ACCEPT_NA_MATCH>(
              left.update_data(reencode_char(left.get_data())),
              right.update_data(reencode_char(right.get_data())),
              warn_
            );
        }
      } else if (!lhs_factor && !rhs_factor) {
        return new JoinVisitorImpl<INTSXP, INTSXP, ACCEPT_NA_MATCH>(left, right, warn_);
      }
      break;
    }
    case REALSXP:
    {
      if (!lhs_factor && is_bare_vector(right.get_data())) {
        return new JoinVisitorImpl<INTSXP, REALSXP, ACCEPT_NA_MATCH>(left, right, warn_);
      }
      break;
      // what else: perhaps we can have INTSXP which is a Date and REALSXP which is a Date too ?
    }
    case LGLSXP:
    {
      if (!lhs_factor) {
        return new JoinVisitorImpl<INTSXP, LGLSXP, ACCEPT_NA_MATCH>(left, right, warn_);
      }
      break;
    }
    case STRSXP:
    {
      if (lhs_factor) {
        warn_bad_var(
          left.get_name(), right.get_name(),
          "joining factor and character vector, coercing into character vector",
          warn_
        );
        return
          new JoinVisitorImpl<STRSXP, STRSXP, ACCEPT_NA_MATCH>(
            left.update_data(reencode_char(left.get_data())),
            right.update_data(reencode_char(right.get_data())),
            warn_
          );
      }
    }
    default:
      break;
    }
    break;
  }
  case REALSXP:
  {
    switch (TYPEOF(right.get_data())) {
    case REALSXP:
      return new JoinVisitorImpl<REALSXP, REALSXP, ACCEPT_NA_MATCH>(left, right, warn_);
    case INTSXP:
      return new JoinVisitorImpl<REALSXP, INTSXP, ACCEPT_NA_MATCH>(left, right, warn_);
    default:
      break;
    }

  }
  case LGLSXP:
  {
    switch (TYPEOF(right.get_data())) {
    case LGLSXP:
      return new JoinVisitorImpl<LGLSXP, LGLSXP, ACCEPT_NA_MATCH> (left, right, warn_);
    case INTSXP:
      return new JoinVisitorImpl<LGLSXP, INTSXP, ACCEPT_NA_MATCH>(left, right, warn_);
    case REALSXP:
      return new JoinVisitorImpl<LGLSXP, REALSXP, ACCEPT_NA_MATCH>(left, right, warn_);
    default:
      break;
    }
    break;
  }
  case STRSXP:
  {
    switch (TYPEOF(right.get_data())) {
    case INTSXP:
    {
      if (Rf_inherits(right.get_data(), "factor")) {
        warn_bad_var(
          left.get_name(), right.get_name(),
          "joining character vector and factor, coercing into character vector",
          warn_
        );
        return
          new JoinVisitorImpl<STRSXP, STRSXP, ACCEPT_NA_MATCH>(
            left.update_data(reencode_char(left.get_data())),
            right.update_data(reencode_char(right.get_data())),
            warn_
          );
      }
      break;
    }
    case STRSXP:
    {
      return
        new JoinVisitorImpl<STRSXP, STRSXP, ACCEPT_NA_MATCH>(
          left.update_data(reencode_char(left.get_data())),
          right.update_data(reencode_char(right.get_data())),
          warn_
        );
    }
    default:
      break;
    }
    break;
  }
  case RAWSXP:
  {
    switch (TYPEOF(right.get_data())) {
    case RAWSXP:
    {
      return new JoinVisitorImpl<RAWSXP, RAWSXP, ACCEPT_NA_MATCH> (left, right, warn_);
    }
    default:
      break;
    }
  }
  default:
    break;
  }

  Rcpp::stop(
    "Can't join on '%s' x '%s' because of incompatible types (%s / %s)",
    left.get_name().get_utf8_cstring(), right.get_name().get_utf8_cstring(),
    get_single_class(left.get_data()), get_single_class(right.get_data())
  );
}

JoinVisitor* join_visitor(const Column& left, const Column& right, bool warn, bool accept_na_match) {
  if (accept_na_match)
    return join_visitor<true>(left, right, warn);
  else
    return join_visitor<false>(left, right, warn);
}

}
