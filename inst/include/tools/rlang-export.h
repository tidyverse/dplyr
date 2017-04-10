#ifndef RLANG_EXPORT_H
#define RLANG_EXPORT_H

#define R_NO_REMAP
#include <Rinternals.h>

typedef void* (*DL_FUNC)();
typedef union {void* p; DL_FUNC fn;} fn_ptr;

SEXP rlang_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot);
DL_FUNC rlang_ExternalPtrAddrFn(SEXP s);
void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn);

#endif
