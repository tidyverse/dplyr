#' @description
#' To learn more about dplyr, start with the vignettes:
#' `browseVignettes(package = "dplyr")`
#' @useDynLib dplyr, .registration = TRUE
#' @keywords internal
#' @import rlang
#' @rawNamespace import(vctrs, except = data_frame)
#' @importFrom glue glue glue_collapse glue_data
#' @importFrom stats setNames update
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom lifecycle deprecated
"_PACKAGE"

# We're importing vctrs without `data_frame()` because we currently
# reexport the deprecated `tibble::data_frame()` function
