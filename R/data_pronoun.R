#' @export
`$.dplyr_data_pronoun` <- function(x, name) {
  dollar_data_pronoun(x, sym(name))
}
#' @export
`[[.dplyr_data_pronoun` <- function(x, i, ...) {
  if (!is_string(i)) {
    abort("Must subset the data pronoun with a string")
  }
  dollar_data_pronoun(x, sym(i))
}

#' @export
`$<-.dplyr_data_pronoun` <- function(x, i, value) {
  abort("Can't modify the data pronoun")
}
#' @export
`[[<-.dplyr_data_pronoun` <- function(x, i, value) {
  abort("Can't modify the data pronoun")
}


#' @export
names.dplyr_data_pronoun <- function(x) {
  # the second environment (active) has all the active bindings
  # so we only need to look there
  names(unclass(x)[[2]])
}
#' @export
length.dplyr_data_pronoun <- function(x) {
  length(unclass(x)[[2]])
}

#' @export
print.dplyr_data_pronoun <- function(x, ...) {
  objs <- glue_countable(length(x), "object")
  cat(paste0("<pronoun>\n", objs, "\n"))
  invisible(x)
}
#' @importFrom utils str
#' @export
str.dplyr_data_pronoun <- function(object, ...) {
  str(unclass(object)[[2]], ...)
}

glue_countable <- function(n, str) {
  if (n == 1) {
    paste0(n, " ", str)
  } else {
    paste0(n, " ", str, "s")
  }
}
