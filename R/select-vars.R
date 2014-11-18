#' Select variables.
#'
#' These functions power \code{\link{select}()} and \code{\link{rename}()}.
#'
#' @param vars A character vector of existing column names.
#' @param ...,args Expressions to compute. \code{select_vars} and
#'   \code{rename_vars}
#' @param include,exclude Character vector of column names to always
#'   include/exclude.
#' @export
#' @keywords internal
#' @return A named character vector. Values are existing column names,
#'   names are new names.
#' @examples
#' # Keep variables
#' select_vars(names(iris), everything())
#' select_vars(names(iris), starts_with("Petal"))
#' select_vars(names(iris), ends_with("Width"))
#' select_vars(names(iris), contains("etal"))
#' select_vars(names(iris), matches(".t."))
#' select_vars(names(iris), Petal.Length, Petal.Width)
#' select_vars(names(iris), one_of("Petal.Length", "Petal.Width"))
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)]
#' select_vars(names(df), num_range("V", 4:6))
#'
#' # Drop variables
#' select_vars(names(iris), -starts_with("Petal"))
#' select_vars(names(iris), -ends_with("Width"))
#' select_vars(names(iris), -contains("etal"))
#' select_vars(names(iris), -matches(".t."))
#' select_vars(names(iris), -Petal.Length, -Petal.Width)
#'
#' # Rename variables
#' select_vars(names(iris), petal_length = Petal.Length)
#' select_vars(names(iris), petal = starts_with("Petal"))
#'
#' # Rename variables preserving all existing
#' rename_vars(names(iris), petal_length = Petal.Length)
#'
#' # Standard evaluation -------------------------------------------------------
#' # You can use names, calls, formulas (or lists of), or a character vector
#' select_vars_(names(iris), list(~Petal.Length))
#' select_vars_(names(iris), list(quote(Petal.Length)))
#' select_vars_(names(iris), "Petal.Length")
select_vars <- function(vars, ..., include = character(), exclude = character()) {
  args <- lazyeval::lazy_dots(...)
  select_vars_(vars, args, include = include, exclude = exclude)
}

#' @rdname select_vars
#' @export
select_vars_ <- function(vars, args, include = character(), exclude = character()) {

  if (length(args) == 0) {
    vars <- setdiff(include, exclude)
    return(setNames(vars, vars))
  }

  args <- lazyeval::as.lazy_dots(args)

  # No non-standard evaluation - but all names mapped to their position.
  # Keep integer semantics: include = +, exclude = -
  names_list <- setNames(as.list(seq_along(vars)), vars)

  select_funs <- list(
    starts_with = function(...) starts_with(vars, ...),
    ends_with = function(...) ends_with(vars, ...),
    contains = function(...) contains(vars, ...),
    matches = function(...) matches(vars, ...),
    num_range = function(...) num_range(vars, ...),
    one_of = function(...) one_of(vars, ...),
    everything = function(...) everything(vars, ...)
  )

  ind_list <- lazyeval::lazy_eval(args, c(names_list, select_funs))
  names(ind_list) <- names2(args)

  ind <- unlist(ind_list)
  incl <- ind[ind > 0]

  # If only negative values, implicitly assumes all variables to be removed.
  if (sum(ind > 0) == 0 && sum(ind < 0) > 0) {
    incl <- seq_along(vars)
  }
  # Remove duplicates (unique loses names)
  incl <- incl[!duplicated(incl)]

  # Remove variables to be excluded (setdiff loses names)
  excl <- abs(ind[ind < 0])
  incl <- incl[match(incl, excl, 0L) == 0L]

  bad_idx <- incl < 0 | incl > length(vars)
  if (any(bad_idx)) {
    stop("Bad indices: ", paste0(which(bad_idx), collapse = ", "),
      call. = FALSE)
  }
  # Include/exclude specified variables
  sel <- setNames(vars[incl], names(incl))
  sel <- c(setdiff2(include, sel), sel)
  sel <- setdiff2(sel, exclude)


  # Ensure all output vars named
  if (length(sel) == 0) {
    names(sel) <- sel
  } else {
    unnamed <- names2(sel) == ""
    names(sel)[unnamed] <- sel[unnamed]
  }

  sel
}


setdiff2 <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

#' @export
#' @rdname select_vars
rename_vars <- function(vars, ...) {
  rename_vars_(vars, lazyeval::lazy_dots(...))
}

#' @export
#' @rdname select_vars
rename_vars_ <- function(vars, args) {
  if (any(names2(args) == "")) {
    stop("All arguments to rename must be named.", call. = FALSE)
  }

  args <- lazyeval::as.lazy_dots(args)
  is_name <- vapply(args, function(x) is.name(x$expr), logical(1))
  if (!all(is_name)) {
    stop("Arguments to rename must be unquoted variable names. ",
      "Arguments ", paste0(names(args)[!is_name], collapse =", "), " are not.",
      call. = FALSE
    )
  }

  old_vars <- vapply(args, function(x) as.character(x$expr), character(1))
  new_vars <- names(args)

  unknown_vars <- setdiff(old_vars, vars)
  if (length(unknown_vars) > 0) {
    stop("Unknown variables: ", paste0(unknown_vars, collapse = ", "), ".",
      call. = FALSE)
  }

  select <- setNames(vars, vars)
  names(select)[match(old_vars, vars)] <- new_vars

  select
}
