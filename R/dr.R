Rcpp_version <- utils::packageVersion("Rcpp")
R_version <- R.version.string

#' Dr Dplyr checks your installation for common problems.
#'
#' Only run this if you are seeing problems, like random crashes.
#' It's possible for `dr_dplyr` to return false positives, so there's no
#' need to run if all is ok.
#'
#' @export
#' @examples
#' \dontrun{
#' dr_dplyr()
#' }
dr_dplyr <- function() {
  if (Rcpp_version != utils::packageVersion("Rcpp")) {
    warning(
      "Installed Rcpp (", utils::packageVersion("Rcpp"), ") different from ",
      "Rcpp used to build dplyr (", Rcpp_version, ").\n",
      "Please reinstall dplyr to avoid random crashes or undefined behavior.",
      call. = FALSE
    )
  }

  if (R_version != R.version.string) {
    warning(
      "Installed R (", R.version.string, ") different from ",
      "R used to build dplyr (", R_version, ").\n",
      "Please reinstall dplyr to avoid random crashes or undefined behavior.",
      call. = FALSE
    )
  }

  invisible(NULL)
}
