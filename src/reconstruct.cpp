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
//   if it can make efficient internal `row.names`. Again, this materializes
//   lazy ALTREP `row.names`, like those used by duckplyr.
//
// We avoid this by:
// - Using `R_mapAttrib()`, which iterates over the `ATTRIB()` pairlist rather
//   than using `Rf_getAttrib()`.
// - Using `Rf_setAttrib()` for all attributes except `names` and `row.names`,
//   because we retain `data`'s version of these.
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

struct cb_data {
  SEXP out;
  bool seen_names;
  bool seen_row_names;
};

SEXP cb_clear(SEXP tag, SEXP value, void* data) {
  struct cb_data* p_data = (struct cb_data*) data;

  // Retain `data`'s `names` and `row.names`
  if (tag == R_NamesSymbol) {
    p_data->seen_names = true;
    return NULL;
  }
  if (tag == R_RowNamesSymbol) {
    p_data->seen_row_names = true;
    return NULL;
  }

  // Clear all other `data` attributes
  Rf_setAttrib(p_data->out, tag, R_NilValue);

  return NULL;
}

SEXP cb_restore(SEXP tag, SEXP value, void* data) {
  struct cb_data* p_data = (struct cb_data*) data;

  // Skip `template_`'s `names` and `row.names`
  if (tag == R_NamesSymbol) {
    p_data->seen_names = true;
    return NULL;
  }
  if (tag == R_RowNamesSymbol) {
    p_data->seen_row_names = true;
    return NULL;
  }

  // Install all other `template_` attributes
  Rf_setAttrib(p_data->out, tag, value);

  return NULL;
}

SEXP ffi_dplyr_reconstruct(SEXP data, SEXP template_) {
  if (TYPEOF(data) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must be a list.");
  }
  if (TYPEOF(template_) != VECSXP) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must be a list.");
  }
  if (!Rf_isObject(data)) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must be an object.");
  }
  if (!Rf_isObject(template_)) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must be an object.");
  }

  // Shallow duplicates attributes as well
  SEXP out = PROTECT(Rf_shallow_duplicate(data));

  // Clear all `data` attributes except `names` and `row.names`.
  // Iterate over `data` attributes so we can modify `out`'s in place.
  // (Can't use nice named `.seen_names = false` syntax because that's
  // apparently a C++20 feature even though C99 supports it)
  struct cb_data clear_data = {
    out,
    false,
    false
  };
  struct cb_data* p_clear_data = &clear_data;
  R_mapAttrib(data, cb_clear, (void*) p_clear_data);

  // Restore all `template_` attributes except `names` and `row.names`
  struct cb_data restore_data = {
    out,
    false,
    false
  };
  struct cb_data* p_restore_data = &restore_data;
  R_mapAttrib(template_, cb_restore, (void*) p_restore_data);

  // Sanity checks
  if (!p_clear_data->seen_names) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must have a `names` attribute.");
  }
  if (!p_clear_data->seen_row_names) {
    Rf_errorcall(R_NilValue, "Internal error: `data` must have a `row.names` attribute.");
  }
  if (!p_restore_data->seen_names) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must have a `names` attribute.");
  }
  if (!p_restore_data->seen_row_names) {
    Rf_errorcall(R_NilValue, "Internal error: `template` must have a `row.names` attribute.");
  }

  UNPROTECT(1);
  return out;
}

// Very unsafe wrappers needed for testing.
// Bypass `Rf_getAttrib()` and `Rf_setAttrib()` calls to avoid forcing ALTREP
// `row.names`. Can't use on R >=4.6.0, but there is currently no way to install
// row names without materializing them without using `SET_ATTRIB()`.
SEXP ffi_test_dplyr_attributes(SEXP x) {
#if (R_VERSION < R_Version(4, 6, 0))
  return ATTRIB(x);
#else
  Rf_errorcall(R_NilValue, "Internal error: Can't call this on R >=4.6.0");
#endif
}
SEXP ffi_test_dplyr_set_attributes(SEXP x, SEXP attributes) {
#if (R_VERSION < R_Version(4, 6, 0))
  if (TYPEOF(attributes) != LISTSXP) {
    Rf_errorcall(R_NilValue, "`attributes` must be a pairlist.");
  }
  x = PROTECT(Rf_shallow_duplicate(x));
  SET_ATTRIB(x, attributes);
  UNPROTECT(1);
  return x;
#else
  Rf_errorcall(R_NilValue, "Internal error: Can't call this on R >=4.6.0");
#endif
}
