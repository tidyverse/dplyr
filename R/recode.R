#' Recode values
#'
#' This is a vectorised version of \code{\link{switch}()}: you can replace
#' numeric values based on their position, and character values by their
#' name. This is an S3 generic: dplyr provides methods for numeric, character,
#' and factors. For logical vectors, use \code{\link{if_else}}
#'
#' @param .x A vector to modify
#' @param ... Replacments. These should be named for character and factor
#'   \code{.x}, and can be named for numeric \code{.x}.
#'
#'   All replacements must be the same type, and must have either
#'   length one or the same length as x.
#' @param .default If supplied, all values not otherwise matched will
#'   be given this value. If not supplied and if the replacements are
#'   the same type as the original values in \code{.x}, unmatched
#'   values are not changed. If not supplied and if the replacements
#'   are not compatible, unmatched values are replaced with \code{NA}.
#'   \code{.default} must be either length 1 or the same length as
#'   \code{.x}.
#' @param .missing If supplied, any missing values in \code{.x} will be
#'   replaced by this value. Must be either length 1 or the same length as
#'   \code{.x}.
#' @param .ordered If \code{TRUE}, \code{recode_factor()} creates an
#'   ordered factor.
#' @return A vector the same length as \code{.x}, and the same type as
#'   the first of \code{...}, \code{.default}, or \code{.missing}.
#'   \code{recode_factor()} returns a factor whose levels are in the
#'   same order as in \code{...}.
#' @export
#' @examples
#' # Recode values with named arguments
#' x <- sample(c("a", "b", "c"), 10, replace = TRUE)
#' recode(x, a = "Apple")
#' recode(x, a = "Apple", .default = NA_character_)
#'
#' # Named arguments also work with numeric values
#' x <- c(1:5, NA)
#' recode(x, `2` = 20L, `4` = 40L)
#'
#' # Note that if the replacements are not compatible with .x,
#' # unmatched values are replaced by NA and a warning is issued.
#' recode(x, `2` = "b", `4` = "d")
#'
#' # If you don't name the arguments, recode() matches by position
#' recode(x, "a", "b", "c")
#' recode(x, "a", "b", "c", .default = "other")
#' recode(x, "a", "b", "c", .default = "other", .missing = "missing")
#'
#' # Supply default with levels() for factors
#' x <- factor(c("a", "b", "c"))
#' recode(x, a = "Apple", .default = levels(x))
#'
#' # Use recode_factor() to create factors with levels ordered as they
#' # appear in the recode call. The levels in .default and .missing
#' # come last.
#' x <- c(1:4, NA)
#' recode_factor(x, `1` = "z", `2` = "y", `3` = "x")
#' recode_factor(x, `1` = "z", `2` = "y", .default = "D")
#' recode_factor(x, `1` = "z", `2` = "y", .default = "D", .missing = "M")
#'
#' # When the input vector is a compatible vector (character vector or
#' # factor), it is reused as default.
#' recode_factor(letters[1:3], b = "z", c = "y")
#' recode_factor(factor(letters[1:3]), b = "z", c = "y")
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  UseMethod("recode")
}

#' @export
recode.numeric <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- list(...)

  nms <- has_names(values)
  if (all(nms)) {
    vals <- as.double(names(values))
  } else if (all(!nms)) {
    vals <- seq_along(values)
  } else {
    stop("Either all values must be named, or none must be named.",
      call. = FALSE)
  }

  n <- length(.x)
  template <- find_template(..., .default, .missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (i in seq_along(values)) {
    out <- replace_with(out, .x == vals[i], values[[i]], paste0("Vector ", i))
    replaced[.x == vals[i]] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced & !is.na(.x), .default, "`.default`")
  out <- replace_with(out, is.na(.x), .missing, "`.missing`")
  out
}

#' @export
recode.character <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- list(...)
  if (!all(has_names(values))) {
    stop("All replacements must be named", call. = FALSE)
  }

  n <- length(.x)
  template <- find_template(..., .default, .missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in names(values)) {
    out <- replace_with(out, .x == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[.x == nm] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced & !is.na(.x), .default, "`.default`")
  out <- replace_with(out, is.na(.x), .missing, "`.missing`")
  out
}

#' @export
recode.factor <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- list(...)
  if (length(values) == 0) {
    stop("No replacements provided", call. = FALSE)
  }

  if (!all(has_names(values))) {
    stop("All replacements must be named", call. = FALSE)
  }
  if (!is.null(.missing)) {
    stop("`missing` is not supported for factors", call. = FALSE)
  }

  out <- rep(NA_character_, length(levels(.x)))
  replaced <- rep(FALSE, length(levels(.x)))

  for (nm in names(values)) {
    out <- replace_with(out, levels(.x) == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[levels(.x) == nm] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced, .default, "`.default`")
  levels(.x) <- out

  .x
}

find_template <- function(...) {
  x <- compact(list(...))

  if (length(x) == 0) {
    stop("No replacements provided", call. = FALSE)
  }

  x[[1]]
}

validate_recode_default <- function(default, x, out, replaced) {
  default <- recode_default(x, default, out)

  if (is.null(default) && sum(replaced & !is.na(x)) < length(out[!is.na(x)])) {
    warning("Unreplaced values treated as NA as .x is not compatible. ",
      "Please specify replacements exhaustively or supply .default",
      call. = FALSE)
  }

  default
}

recode_default <- function(x, default, out) {
  UseMethod("recode_default")
}

recode_default.default <- function(x, default, out) {
  same_type <- identical(typeof(x), typeof(out))
  if (is.null(default) && same_type) {
    x
  } else {
    default
  }
}

recode_default.factor <- function(x, default, out) {
  if (is.null(default) && is.factor(x)) {
    levels(x)
  } else {
    default
  }
}

#' @rdname recode
#' @export
recode_factor <- function (.x, ..., .default = NULL, .missing = NULL,
                           .ordered = FALSE) {
  recoded <- recode(.x, ..., .default = .default, .missing = .missing)

  all_levels <- unique(c(..., recode_default(.x, .default, recoded), .missing))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}
