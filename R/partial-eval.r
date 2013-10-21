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
#' \code{partial_eval} needs to guess if you're referring to a variable on the
#' server (remote), or in the current environment (local). It's not possible to
#' do this 100% perfectly. \code{partial_eval} uses the following heuristic:
#'
#' \itemize{
#'   \item If the source variables are known, and the symbol matches a source
#'     variable, then remote.
#'   \item If the symbol is defined locally, local.
#'   \item Otherwise, remote.
#' }
#'
#' @param call an unevaluated expression, as produced by \code{\link{quote}}
#' @param source a tbl object
#' @param env environment in which to search for local values
#' @export
#' @keywords internal
#' @examples
#' data("Batting", package = "Lahman")
#' bdf <- tbl_df(Batting)
#' partial_eval(quote(year > 1980), bdf)
#'
#' ids <- c("ansonca01", "forceda01", "mathebo01")
#' partial_eval(quote(id %in% ids), bdf)
#'
#' # You can use local to disambiguate between local and remote
#' # variables: otherwise remote is always preferred
#' year <- 1980
#' partial_eval(quote(year > year), bdf)
#' partial_eval(quote(year > local(year)), bdf)
#'
#' # Functions are always assumed to be remote. Use local to force evaluation
#' # in R.
#' f <- function(x) x + 1
#' partial_eval(quote(year > f(1980)), bdf)
#' partial_eval(quote(year > local(f(1980))), bdf)
#'
#' # For testing you can also use it with the source omitted
#' partial_eval(quote(1 + 2 * 3))
#' x <- 1
#' partial_eval(quote(x ^ y))
partial_eval <- function(call, source = NULL, env = parent.frame()) {
  if (is.atomic(call)) return(call)

  if (is.list(call)) {
    lapply(call, partial_eval, source = source, env = env)
  } else if (is.symbol(call)) {
    name <- as.character(call)
    if (!is.null(source) && name %in% tbl_vars(source)) {
      call
    } else if (exists(name, env)) {
      eval(call, env)
    } else {
      call
    }
  } else if (is.call(call)) {
    # Process call arguments recursively, unless user has manually called
    # remote/local
    name <- as.character(call[[1]])
    if (name == "local") {
      eval(call[[2]], env)
    } else if (name == "remote") {
      call[[2]]
    } else {
      call[-1] <- lapply(call[-1], partial_eval, source = source, env = env)
      call
    }
  } else {
    stop("Unknown input type: ", class(call), call. = FALSE)
  }
}

#' Evaluate variable names in the context of a tbl.
#'
#' @param exprs a list of unevaluated expressions
#' @param tbl,select a tbl or a select language list
#' @param parent the parent frame in which to evaluate variables/functions
#'   not found in \code{tbl}
#' @export
#' @examples
#' var_eval(list(quote(mpg:wt)), mtcars)
#' 
#' select <- lapply(names(mtcars), as.name)
#' select_eval(list(quote(mpg:wt)), select)
#' 
#' mutate <- c(select, cyl2 = quote(cyl * 2))
#' select_eval(list(quote(gear:cyl2)), mutate)
var_eval <- function(exprs, tbl, parent = parent.frame()) {
  nm <- tbl_vars(tbl)
  
  nms_list <- as.list(setNames(seq_along(nm), nm))

  idx <- lapply(exprs, eval, nms_list, parent)
  symbols <- lapply(nm, as.symbol)
  
  symbols[unlist(idx)]
}

#' @rdname var_eval
#' @export
select_eval <- function(exprs, select, parent = parent.frame()) {
  nms_list <- as.list(setNames(seq_along(select), auto_names(select)))
  
  idx <- lapply(exprs, eval, nms_list, parent)
  
  select[unlist(idx)]
}

#' @rdname var_eval
#' @export
var_index <- function(exprs, tbl, parent = parent.frame()) {
  nm <- names(tbl)
  nms_list <- as.list(setNames(seq_along(nm), nm))
  
  unlist(lapply(exprs, eval, nms_list, parent))
}

