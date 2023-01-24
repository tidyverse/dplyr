#' Rename columns
#'
#' `rename()` changes the names of individual variables using
#' `new_name = old_name` syntax; `rename_with()` renames columns using a
#' function.
#'
#' @inheritParams arrange
#' @param ...
#'   For `rename()`: <[`tidy-select`][dplyr_tidy_select]> Use
#'   `new_name = old_name` to rename selected variables.
#'
#'   For `rename_with()`: additional arguments passed onto `.fn`.
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * Rows are not affected.
#' * Column names are changed; column order is preserved.
#' * Data frame attributes are preserved.
#' * Groups are updated to reflect new names.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("rename")}.
#' @family single table verbs
#' @export
#' @examples
#' iris <- as_tibble(iris) # so it prints a little nicer
#' rename(iris, petal_length = Petal.Length)
#'
#' # Rename using a named vector and `all_of()`
#' lookup <- c(pl = "Petal.Length", sl = "Sepal.Length")
#' rename(iris, all_of(lookup))
#'
#' # If your named vector might contain names that don't exist in the data,
#' # use `any_of()` instead
#' lookup <- c(lookup, new = "unknown")
#' try(rename(iris, all_of(lookup)))
#' rename(iris, any_of(lookup))
#'
#' rename_with(iris, toupper)
#' rename_with(iris, toupper, starts_with("Petal"))
#' rename_with(iris, ~ tolower(gsub(".", "_", .x, fixed = TRUE)))
#' @export
rename <- function(.data, ...) {
  UseMethod("rename")
}

#' @export
rename.data.frame <- function(.data, ...) {
  loc <- tidyselect::eval_rename(expr(c(...)), .data)
  # eval_rename() only returns changes
  names <- names(.data)
  names[loc] <- names(loc)

  set_names(.data, names)
}

#' @export
#' @rdname rename
#' @param .fn A function used to transform the selected `.cols`. Should
#'   return a character vector the same length as the input.
#' @param .cols <[`tidy-select`][dplyr_tidy_select]> Columns to rename;
#'   defaults to all columns.
rename_with <- function(.data, .fn, .cols = everything(), ...) {
  UseMethod("rename_with")
}


#' @export
rename_with.data.frame <- function(.data, .fn, .cols = everything(), ...) {
  .fn <- as_function(.fn)
  cols <- tidyselect::eval_select(enquo(.cols), .data, allow_rename = FALSE)

  names <- names(.data)

  sel <- vec_slice(names, cols)
  new <- .fn(sel, ...)

  if (!is_character(new)) {
    cli::cli_abort(
      "{.arg .fn} must return a character vector, not {.obj_type_friendly {new}}."
    )
  }
  if (length(new) != length(sel)) {
    cli::cli_abort(
      "{.arg .fn} must return a vector of length {length(sel)}, not {length(new)}."
    )
  }

  names <- vec_assign(names, cols, new)
  names <- vec_as_names(names, repair = "check_unique")

  set_names(.data, names)
}
