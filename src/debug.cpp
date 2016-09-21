#include <Rcpp.h>
#include <tools/Encoding.h>
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector strings_addresses(CharacterVector s) {
  static char buffer[20];
  int n = s.size();

  CharacterVector res(n);
  for (int i=0; i<n; i++) {
    SEXP x = s[i];
    snprintf(buffer, 20, "%p", reinterpret_cast<void*>(x));
    res[i] = buffer;
  }
  res.names() = s;

  return res;
}

// simple internal debugging function to access the gp part of the SEXP
// only meant for internal use in dplyr debugging

// [[Rcpp::export]]
unsigned short gp(SEXP x) {
  return reinterpret_cast<sxpinfo_struct*>(x)->gp;
}
