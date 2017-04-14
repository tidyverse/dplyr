#ifndef DPLYR_DPLYR_BAD_H
#define DPLYR_DPLYR_BAD_H

namespace dplyr {

template<class C1>
void NORET bad_arg(const SymbolString& arg, C1 arg1) {
  Function bad_fun = Function("bad_args", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(arg.get_string()), wrap(arg1));
  stop("");
}

template<class C1, class C2>
void NORET bad_arg(const SymbolString& arg, C1 arg1, C2 arg2) {
  Function bad_fun = Function("bad_args", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(arg.get_string()), wrap(arg1), arg2);
  stop("");
}

template<class C1, class C2, class C3>
void NORET bad_arg(const SymbolString& arg, C1 arg1, C2 arg2, C3 arg3) {
  Function bad_fun = Function("bad_args", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(arg.get_string()), wrap(arg1), arg2, arg3);
  stop("");
}

template<class C1>
void NORET bad_pos_arg(int pos_arg, C1 arg1) {
  Function bad_fun = Function("bad_pos_args", Environment::namespace_env("dplyr"));
  bad_fun(wrap(pos_arg), wrap(arg1));
  stop("");
}

template<class C1, class C2>
void NORET bad_pos_arg(int pos_arg, C1 arg1, C2 arg2) {
  Function bad_fun = Function("bad_pos_args", Environment::namespace_env("dplyr"));
  bad_fun(wrap(pos_arg), wrap(arg1), arg2);
  stop("");
}

template<class C1, class C2, class C3>
void NORET bad_pos_arg(int pos_arg, C1 arg1, C2 arg2, C3 arg3) {
  Function bad_fun = Function("bad_pos_args", Environment::namespace_env("dplyr"));
  bad_fun(wrap(pos_arg), wrap(arg1), arg2, arg3);
  stop("");
}

template<class C1>
void NORET bad_col(const SymbolString& col, C1 arg1) {
  Function bad_fun = Function("bad_cols", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(col.get_string()), wrap(arg1));
  stop("");
}

template<class C1, class C2>
void NORET bad_col(const SymbolString& col, C1 arg1, C2 arg2) {
  Function bad_fun = Function("bad_cols", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(col.get_string()), wrap(arg1), arg2);
  stop("");
}

template<class C1, class C2, class C3>
void NORET bad_col(const SymbolString& col, C1 arg1, C2 arg2, C3 arg3) {
  Function bad_fun = Function("bad_cols", Environment::namespace_env("dplyr"));
  bad_fun(CharacterVector::create(col.get_string()), wrap(arg1), arg2, arg3);
  stop("");
}

}

#endif // DPLYR_DPLYR_BAD_H
