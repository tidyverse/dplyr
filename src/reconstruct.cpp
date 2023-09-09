#include "dplyr.h"

// mostly installAttrib from attrib.c
void set_attrib(SEXP s, SEXP name, SEXP val) {
    SEXP last_attrib = R_NilValue;
    for (SEXP attrib = ATTRIB(s); attrib != R_NilValue; attrib = CDR(attrib)) {
        last_attrib = attrib;
    }
    SEXP new_attrib = PROTECT(Rf_cons(val, R_NilValue));
    SET_TAG(new_attrib, name);
    if (ATTRIB(s) == R_NilValue) {
        SET_ATTRIB(s, new_attrib);
    } else  {
        SETCDR(last_attrib, new_attrib);
    }
    UNPROTECT(1);
}

SEXP dplyr_reconstruct_impl(SEXP result, SEXP template_) {
    if (result == R_NilValue || result == R_NilValue) {
        Rf_error("Need non-NULL parameters");
    }

    // get existing names and row names, circumventing R's logic
    SEXP names = R_NilValue;
    SEXP row_names = R_NilValue;

    for (SEXP attr = ATTRIB(result); attr != R_NilValue; attr = CDR(attr)) {
        SEXP tag = TAG(attr);
        if (tag == R_NamesSymbol) {
            names = CAR(attr);
        } else if (tag == R_RowNamesSymbol) {
            row_names = CAR(attr);
        }
    }

    // clear all attributes on target
    SET_ATTRIB(result, R_NilValue);

    // add attributes from template that are *not* names or row.names
    for (SEXP attr = ATTRIB(template_); attr != R_NilValue; attr = CDR(attr)) {
        SEXP tag = TAG(attr);
        if (tag == R_NamesSymbol) {
            set_attrib(result, tag, names);
            names = R_NilValue;
        } else if (tag == R_RowNamesSymbol) {
            set_attrib(result, tag, row_names);
            row_names = R_NilValue;
        } else {
            set_attrib(result, tag, CAR(attr));
        }
    }

    // restore names and row names if not done yet
    if (names != R_NilValue) {
        set_attrib(result, R_NamesSymbol, names);
    }
    if (row_names != R_NilValue) {
        set_attrib(result, R_RowNamesSymbol, row_names);
    }

    return result;
}
