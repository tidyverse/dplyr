#ifndef dplyr_promise_H
#define dplyr_promise_H

#include <R.h>

namespace dplyr {

class promise {
public:

  promise(SEXP name_, SEXP env_, SEXP expr) :
    name(name_),
    symb_name(Rf_installChar(name_)),
    env(env_)
  {
    install(expr);
  }

  inline void install(SEXP expr) {
    delayedAssign(expr);
    prom = Rf_findVarInFrame(env, symb_name);
  }

  inline bool was_forced() {
    return PRVALUE(prom) != R_UnboundValue;
  }

  inline SEXP code() {
    return PRCODE(prom);
  }

  inline SEXP value() {
    return PRVALUE(prom);
  }

private:
  SEXP name;
  SEXP symb_name;
  SEXP env;
  SEXP prom;

  // delayedAssign( name, call, eval.env = baseenv(), assign_env = env )
  void delayedAssign(SEXP expr) {
    static SEXP symb_delayedAssign = Rf_install("delayedAssign");
    SEXP delayedAssignCall = PROTECT(Rf_lang5(symb_delayedAssign, Rf_ScalarString(name), expr, R_BaseEnv, env));
    PROTECT(Rf_eval(delayedAssignCall, R_BaseEnv));
    UNPROTECT(2);
  }

};

}

#endif
