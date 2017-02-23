#' Partially evaluate an expression.
#'
#' This function partially evaluates an expression, using information from
#' the tbl to determine whether names refer to local expressions
#' or remote variables. This simplifies SQL translation because expressions
#' don't need to carry around their environment - all revelant information
#' is incorporated into the expression.
#'
#' @section Symbol substitution:
#'
#' `partial_eval()` needs to guess if you're referring to a variable on the
#' server (remote), or in the current environment (local). It's not possible to
#' do this 100% perfectly. `partial_eval()` uses the following heuristic:
#'
#' \itemize{
#'   \item If the tbl variables are known, and the symbol matches a tbl
#'     variable, then remote.
#'   \item If the symbol is defined locally, local.
#'   \item Otherwise, remote.
#' }
#'
#' @param call an unevaluated expression, as produced by [quote()]
#' @param vars character vector of variable names.
#' @param env environment in which to search for local values
#' @export
#' @keywords internal
#' @examples
#' vars <- c("year", "id")
#' partial_eval(quote(year > 1980), vars = vars)
#'
#' ids <- c("ansonca01", "forceda01", "mathebo01")
#' partial_eval(quote(id %in% ids), vars = vars)
#'
#' # You can use local to disambiguate between local and remote
#' # variables: otherwise remote is always preferred
#' year <- 1980
#' partial_eval(quote(year > year), vars = vars)
#' partial_eval(quote(year > local(year)), vars = vars)
#'
#' # Functions are always assumed to be remote. Use local to force evaluation
#' # in R.
#' f <- function(x) x + 1
#' partial_eval(quote(year > f(1980)), vars = vars)
#' partial_eval(quote(year > local(f(1980))), vars = vars)
#'
#' # For testing you can also use it with the tbl omitted
#' partial_eval(quote(1 + 2 * 3))
#' x <- 1
#' partial_eval(quote(x ^ y))
partial_eval <- function(call, vars = character(), env = parent.frame()) {
  if (is.atomic(call)) return(call)

  if (inherits(call, "lazy_dots")) {
    lapply(call, function(l) partial_eval(l$expr, vars, l$env))
  } else if (is.list(call)) {
    lapply(call, partial_eval, vars, env = env)
  } else if (is.symbol(call)) {
    name <- as.character(call)
    if (name %in% vars) {
      call
    } else if (exists(name, envir = env)) {
      eval(call, env)
    } else {
      call
    }
  } else if (is.call(call)) {
    # If the function "name" is a call to another function, evaluate
    # the whole thing local
    if (is.call(call[[1]])) {
      return(eval(call, env))
    }

    # Process call arguments recursively, unless user has manually called
    # remote/local
    name <- as.character(call[[1]])
    if (name == "local") {
      eval(call[[2]], env)
    } else if (name %in% c("$", "[[", "[")) {
      # Subsetting is always done locally
      eval(call, env)
    } else if (name == "remote") {
      call[[2]]
    } else {
      call[-1] <- lapply(call[-1], partial_eval, vars = vars, env = env)
      call
    }
  } else {
    stop("Unknown input type: ", class(call), call. = FALSE)
  }
}
