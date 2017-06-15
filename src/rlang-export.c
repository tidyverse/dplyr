#define R_NO_REMAP
#include <Rinternals.h>
#include <Rversion.h>

#include <tools/rlang-export.h>

#if (defined(R_VERSION) && R_VERSION < R_Version(3, 4, 0))
SEXP R_MakeExternalPtrFn(DL_FUNC p, SEXP tag, SEXP prot) {
  fn_ptr ptr;
  ptr.fn = p;
  return R_MakeExternalPtr(ptr.p, tag, prot);
}
DL_FUNC R_ExternalPtrAddrFn(SEXP s) {
  fn_ptr ptr;
  ptr.p = EXTPTR_PTR(s);
  return ptr.fn;
}
#endif

SEXP rlang_namespace(const char* ns) {
  SEXP call = PROTECT(Rf_lang2(Rf_install("getNamespace"), PROTECT(Rf_mkString(ns))));
  SEXP ns_env = Rf_eval(call, R_BaseEnv);
  UNPROTECT(2);
  return ns_env;
}

void rlang_register_pointer(const char* ns, const char* ptr_name, DL_FUNC fn) {
  SEXP ptr = PROTECT(R_MakeExternalPtrFn(fn, R_NilValue, R_NilValue));

  SEXP ptr_obj = PROTECT(Rf_allocVector(VECSXP, 1));
  SET_VECTOR_ELT(ptr_obj, 0, ptr);

  Rf_setAttrib(ptr_obj, R_ClassSymbol, Rf_mkString("fn_pointer"));

  Rf_defineVar(Rf_install(ptr_name), ptr_obj, PROTECT(rlang_namespace(ns)));
  UNPROTECT(3);
}
