#ifndef DPLYR_DPLYR_BAD_H
#define DPLYR_DPLYR_BAD_H

namespace dplyr {

template<class C1>
void NORET bad_pos_arg(int pos_arg, C1 arg1) {
  using Rcpp::_;
  static Rcpp::Function bad_fun = Rcpp::Function("bad_pos_args", Rcpp::Environment::namespace_env("dplyr"));
  static Rcpp::Function identity = Rcpp::Function("identity", Rcpp::Environment::base_env());
  Rcpp::String message = bad_fun(pos_arg, arg1, _[".abort"] = identity);
  message.set_encoding(CE_UTF8);
  Rcpp::stop(message.get_cstring());
}

template<class C1, class C2>
void NORET bad_pos_arg(int pos_arg, C1 arg1, C2 arg2) {
  using Rcpp::_;
  static Rcpp::Function bad_fun = Rcpp::Function("bad_pos_args", Rcpp::Environment::namespace_env("dplyr"));
  static Rcpp::Function identity = Rcpp::Function("identity", Rcpp::Environment::base_env());
  Rcpp::String message = bad_fun(pos_arg, arg1, arg2, _[".abort"] = identity);
  message.set_encoding(CE_UTF8);
  Rcpp::stop(message.get_cstring());
}

template<class C1, class C2, class C3>
void NORET bad_pos_arg(int pos_arg, C1 arg1, C2 arg2, C3 arg3) {
  using Rcpp::_;
  static Rcpp::Function bad_fun = Rcpp::Function("bad_pos_args", Rcpp::Environment::namespace_env("dplyr"));
  static Rcpp::Function identity = Rcpp::Function("identity", Rcpp::Environment::base_env());
  Rcpp::String message = bad_fun(pos_arg, arg1, arg2, arg3, _[".abort"] = identity);
  message.set_encoding(CE_UTF8);
  Rcpp::stop(message.get_cstring());
}

}

#endif // DPLYR_DPLYR_BAD_H
