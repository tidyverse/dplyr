#include "dplyr.h"

// Essentially, a C implementation of:
//
// ```
// attributes <- attributes(template)
// attributes$names <- names(data)
// attributes$row.names <- .row_names_info(data, type = 0L)
// attributes(data) <- attributes
// ```
//
// The problem with that is that:
// - `attributes()` ends up calling `Rf_getAttrib()`, which tries to check
//   for internal `row.names` in `template` so they aren't leaked to the user.
//   Unfortunately this materializes lazy ALTREP `row.names`, like those used
//   by duckplyr.
// - `attributes<-()` ends up calling `Rf_setAttrib()`, which tries to check
//   if it can make efficient internal `row.names`.  Again, this materializes
//   lazy ALTREP `row.names`, like those used by duckplyr.
//
// So we bypass that here by carefully manipulating the attribute pairlists.
//
// We expect that at this point, both `data` and `template_` are S3 data
// frames, both of which have `names` and `row.names` attributes. If this isn't
// true, we error.
// - For `data`, we enforce this in `dplyr_reconstruct()`'s generic by calling
//   `dplyr_new_data_frame()` (ideally no intermediate method invalidates this).
// - For `template_`, we assume this since we got here through the S3 method
//   `dplyr_reconstruct.data.frame()`, which dispatched off `template_`. A
//   well-formed S3 data frame must have `names` and `row.names` attributes.
//
// https://github.com/tidyverse/dplyr/pull/6947
// https://github.com/tidyverse/dplyr/issues/6525#issuecomment-1303619152
// https://github.com/wch/r-source/blob/69b94f0c8ce9b2497f6d7a81922575f6c585b713/src/main/attrib.c#L176-L177
// https://github.com/wch/r-source/blob/69b94f0c8ce9b2497f6d7a81922575f6c585b713/src/main/attrib.c#L57
SEXP ffi_dplyr_reconstruct(SEXP data, SEXP template_) {
  if (TYPEOF(data) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must be a list.");
  }
  if (TYPEOF(template_) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must be a list.");
  }
  if (!OBJECT(data)) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must be an object.");
  }
  if (!OBJECT(template_)) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must be an object.");
  }

  bool seen_names = false;
  bool seen_row_names = false;

  // Pull the `names` and `row.names` off `data`.
  // These are the only 2 attributes from `data` that persist.
  SEXP names = R_NilValue;
  SEXP row_names = R_NilValue;

  for (SEXP node = ATTRIB(data); node != R_NilValue; node = CDR(node)) {
    SEXP tag = TAG(node);

    if (tag == R_NamesSymbol) {
      names = CAR(node);
      MARK_NOT_MUTABLE(names);
      seen_names = true;
    }
    if (tag == R_RowNamesSymbol) {
      row_names = CAR(node);
      MARK_NOT_MUTABLE(row_names);
      seen_row_names = true;
    }
  }

  if (!seen_names) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must have a `names` attribute.");
  }
  if (!seen_row_names) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must have a `row.names` attribute.");
  }

  seen_names = false;
  seen_row_names = false;

  // Now replace the `names` and `row.names` attributes in the `template_`
  // attributes with the ones from `data`. This attribute set becomes the final
  // one we set on `data`.
  SEXP attributes = ATTRIB(template_);
  attributes = PROTECT(Rf_shallow_duplicate(attributes));

  for (SEXP node = attributes; node != R_NilValue; node = CDR(node)) {
    SEXP tag = TAG(node);

    if (tag == R_NamesSymbol) {
      SETCAR(node, names);
      seen_names = true;
    }
    if (tag == R_RowNamesSymbol) {
      SETCAR(node, row_names);
      seen_row_names = true;
    }
  }

  if (!seen_names) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must have a `names` attribute.");
  }
  if (!seen_row_names) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must have a `row.names` attribute.");
  }

  // Make an ALTREP wrapper if possible, since the underlying data doesn't change.
  // Won't actually make an ALTREP wrapper unless there are >64 columns
  // (internally controlled by R).
#if R_VERSION >= R_Version(3, 6, 0)
  data = PROTECT(R_shallow_duplicate_attr(data));
#else
  data = PROTECT(Rf_shallow_duplicate(data));
#endif

  SET_ATTRIB(data, attributes);

  UNPROTECT(2);
  return data;
}

// Very unsafe wrappers needed for testing.
// Bypass `Rf_getAttrib()` and `Rf_setAttrib()` calls to avoid forcing ALTREP
// `row.names`.
SEXP ffi_test_dplyr_attributes(SEXP x) {
  return ATTRIB(x);
}
SEXP ffi_test_dplyr_set_attributes(SEXP x, SEXP attributes) {
  if (TYPEOF(attributes) != LISTSXP) {
    Rf_errorcall(R_NilValue, "`attributes` must be a pairlist.");
  }
  x = PROTECT(Rf_shallow_duplicate(x));
  SET_ATTRIB(x, attributes);
  UNPROTECT(1);
  return x;
}
