#' dplyr: a grammar of data manipulation
#'
#' dplyr provides a flexible grammar of data manipulation. It's the next
#' iteration of plyr, focused on tools for working with data frames (hence the
#' *d* in the name).
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Identify the most important data manipulation verbs and make them
#'   easy to use from R.
#' \item Provide blazing fast performance for in-memory data by writing key
#'   pieces in C++
#' \item Use the same interface to work with data no matter where it's stored,
#'   whether in a data frame, a data table or database.
#' }
#'
#' To learn more about dplyr, start with the vignettes:
#' `browseVignettes(package = "dplyr")`
#'
#' @section Package options:
#' \describe{
#' \item{`dplyr.show_progress`}{Should lengthy operations such as `do()`
#'   show a progress bar? Default: `TRUE`}
#' }
#'
#' @section Package configurations:
#' These can be set on a package-by-package basis, or for the global environment.
#' See [pkgconfig::set_config()] for usage.
#' \describe{
#' \item{`dplyr::na_matches`}{Should `NA` values be matched in data frame joins
#'   by default? Default: `"na"` (for compatibility with dplyr v0.5.0 and earlier,
#'   subject to change), alternative value: `"never"` (the default
#'   for database backends, see [join.tbl_df()]).}
#' }
#'
#' @useDynLib dplyr, .registration = TRUE
#' @keywords internal
#' @import rlang
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom stats setNames update
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom pkgconfig get_config
#' @importFrom lifecycle deprecated
"_PACKAGE"
