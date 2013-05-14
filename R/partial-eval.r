#' Partially evaluate an expression.
#'
#' This function partially evaluates an expression, using information from
#' the data source to determine whether names refer to local expressions
#' or remote variables.
#'
#' @param source a data source object
#' @param call an unevaluated expression, as produced by \code{\link{quote}}
#' @param env environment in which to search for local values
#' @export
#' @keywords internal
#' @examples
#' data("baseball", package = "plyr")
#' bdf <- data_frame_source(baseball)
#' partial_eval(bdf, quote(year > 1980))
#'
#' ids <- c("ansonca01", "forceda01", "mathebo01")
#' partial_eval(bdf, quote(id %in% ids))
#'
#' # You can use local to disambiguate between local and remote
#' # variables: otherwise remote is always preferred
#' year <- 1980
#' partial_eval(bdf, quote(year > year))
#' partial_eval(bdf, quote(year > local(year)))
#'
#' # Local is also needed if you want to call a local function
#' f <- function(x) x + 1
#' partial_eval(bdf, quote(year > f(1980)))
#' partial_eval(bdf, quote(year > local(f(1980))))
partial_eval <- function(source, call, env = parent.frame()) {
  if (is.atomic(call)) return(call)

  if (is.symbol(call)) {
    # Symbols must be resolveable either locally or remotely

    name <- as.character(call)
    if (name %in% source_vars(source)) {
      substitute(remote_var(var), list(var = as.character(call)))
    } else if (exists(name, env)) {
      get(name, env)
    } else {
      stop(name, " not defined locally or in data source", call. = FALSE)
    }
  } else if (is.call(call)) {
    # Process call arguments recursively, unless user has manually called
    # remote/local
    name <- as.character(call[[1]])
    if (name == "local") {
      eval(call[[2]], env)
    } else if (name == "remote") {
      substitute(remote_var(var), list(var = as.character(call[[2]])))
    } else {
      call[-1] <- lapply(call[-1], partial_eval, source = source, env = env)
      call
    }
  } else {
    stop("Unknown input type: ", class(call), call. = FALSE)
  }
}
