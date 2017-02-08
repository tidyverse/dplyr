inlineCxxPlugin <- Rcpp.plugin.maker(
  include.before = "#include <dplyr.h>",
  package = "dplyr",
  LinkingTo = c("Rcpp", "BH", "dplyr")
)
