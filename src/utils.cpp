#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
void list2df(List x, int nrow) {
   int ncol = x.size();
   x.attr("class") = "data.frame";
   x.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);
   if (Rf_isNull(Rf_getAttrib(x, R_NamesSymbol))) {
      stop("List must have 'names' attribute set");
   }
}
