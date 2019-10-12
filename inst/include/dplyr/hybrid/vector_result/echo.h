#ifndef dplyr_hybrid_echo_h
#define dplyr_hybrid_echo_h

#include <dplyr/hybrid/Dispatch.h>

namespace dplyr {
namespace hybrid {

inline SEXP echo(SEXP x, const Summary&) {
  return dplyr::vectors::unbound_sentinel;
}
inline SEXP echo(SEXP x, const Window&) {
  return x;
}
inline SEXP echo(SEXP x, const Match&) {
  return Rf_mkString("echo");
}

}
}
#endif
