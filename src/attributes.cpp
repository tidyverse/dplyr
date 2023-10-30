#include "dplyr.h"

// Push a new attribute at the end of `x`'s attributes.
// Caller is responsible for ensuring the `tag` doesn't already exist.
static inline
SEXP r_attrib_push(SEXP x, SEXP tag, SEXP value) {
  SEXP attributes = Rf_cons(value, ATTRIB(x));
  SET_TAG(attributes, tag);
  SET_ATTRIB(x, attributes);
  return attributes;
}

// Alternative to `attributes()`, but based heavily on the C level
// implementation, `do_attributes()`.
//
// Unlike `attributes()`, this does not inspect the row names in any way (i.e.
// typically to expand compact row names), which has the side effect of
// materializing duckplyr queries.
// https://github.com/tidyverse/dplyr/issues/6525#issuecomment-1303619152
// https://github.com/wch/r-source/blob/69b94f0c8ce9b2497f6d7a81922575f6c585b713/src/main/attrib.c#L176-L177
SEXP ffi_dplyr_attributes(SEXP x) {
  SEXP attributes = ATTRIB(x);
  R_xlen_t size = Rf_xlength(attributes);

  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));

  SEXP names = Rf_allocVector(STRSXP, size);
  Rf_setAttrib(out, R_NamesSymbol, names);

  for (R_xlen_t i = 0; i < size; ++i, attributes = CDR(attributes)) {
    SEXP name = TAG(attributes);

    if (TYPEOF(name) != SYMSXP) {
      // Hopefully never happens, but `do_attributes()` has a special path
      // for this, so we are extra careful not to crash
      Rf_errorcall(R_NilValue, "Unexpected non-symbol attribute pairlist tag.");
    }

    SET_STRING_ELT(names, i, PRINTNAME(name));

    if (name == R_RowNamesSymbol) {
      // Super special path for row names! Row names are passed through as is,
      // with no modification, although we do mark the attribute as not mutable,
      // like `Rf_getAttrib()`.
      SEXP value = CAR(attributes);
      MARK_NOT_MUTABLE(value);
      SET_VECTOR_ELT(out, i, value);
      continue;
    }

    SEXP value = Rf_getAttrib(x, name);
    SET_VECTOR_ELT(out, i, value);
  }

  UNPROTECT(1);
  return out;
}

// Alternative to `attributes<-()`, but based heavily on the C level
// implementation, `do_attributesgets()`.
//
// Unlike `attributes<-()`, this does not inspect the row names in any way (i.e.
// typically to create compact row names), which has the side effect of
// materializing duckplyr queries.
//
// If we want this to be a full replacement, then it should also ensure that the
// `dim` attribute is set before the `dimnames` attribute, but we currently
// never expect those attributes to appear on data frames.
// https://github.com/wch/r-source/blob/69b94f0c8ce9b2497f6d7a81922575f6c585b713/src/main/attrib.c#L1401C39-L1404
SEXP ffi_dplyr_set_attributes(SEXP x, SEXP attributes) {
  if (x == R_NilValue) {
    Rf_errorcall(R_NilValue, "Internal error: `x` can't be `NULL`.");
  }

  // Make an ALTREP wrapper if possible, since the underlying data doesn't change
#if R_VERSION >= R_Version(3, 6, 0)
  x = PROTECT(R_shallow_duplicate_attr(x));
#else
  x = PROTECT(Rf_shallow_duplicate(x));
#endif

  // Remove existing attributes, and unset the object bit.
  // It will be reset by `Rf_setAttrib()` if needed.
  SET_ATTRIB(x, R_NilValue);
  SET_OBJECT(x, 0);

  if (TYPEOF(attributes) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `attributes` must be a list.");
  }

  const SEXP* v_attributes = VECTOR_PTR_RO(attributes);

  R_xlen_t size = Rf_xlength(attributes);
  SEXP names = Rf_getAttrib(attributes, R_NamesSymbol);

  if (TYPEOF(names) != STRSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `attributes` must be named.");
  }

  const SEXP* v_names = STRING_PTR_RO(names);

  // Check that each element of `names` is named
  for (R_xlen_t i = 0; i < size; ++i) {
    SEXP name = v_names[i];

    if (name == NA_STRING || name == R_BlankString) {
      Rf_errorcall(R_NilValue, "All attributes must have names. Attribute %i does not.", i + 1);
    }
  }

  for (R_xlen_t i = 0; i < size; ++i) {
    SEXP name = Rf_installChar(v_names[i]);
    SEXP attribute = v_attributes[i];

    if (name == R_RowNamesSymbol) {
      // Super special path for row names!
      // Row names are pushed on untouched and unchecked.
      r_attrib_push(x, name, attribute);
      continue;
    }

    Rf_setAttrib(x, name, attribute);
  }

  UNPROTECT(1);
  return x;
}
