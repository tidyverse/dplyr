#' dplyr: a grammar of data manipulation
#'
#' dplyr provides a flexible grammar of data manipulation. It's the next
#' iteration of plyr, focused on tools for working with data frames (hence the
#' \emph{d} in the name).
#'
#' It has three main goals:
#'
#' \itemize{
#' \item Identify the most important data manipulation verbs and make them
#'   easy to use from R.
#' \item Provide blazing fast performance for in-memory data by writing key
#'   pieces in C++ (using Rcpp)
#' \item Use the same interface to work with data no matter where it's stored,
#'   whether in a data frame, a data table or database.
#' }
#'
#' To learn more about dplyr, start with the vignettes:
#' \code{browseVignettes(package = "dplyr")}
#'
#' @docType package
#' @name dplyr
#' @useDynLib dplyr
#' @import assertthat
#' @importFrom utils head tail
#' @importFrom Rcpp cppFunction Rcpp.plugin.maker
#' @importFrom stats setNames update
NULL

# Needed for data.table tests, but don't need to export
.datatable.aware <- TRUE
