# compat-name-repair (last updated: tibble 2.0.1.9000)

# This file serves as a reference for compatibility functions for
# name repair in tibble, until name repair is available in rlang.

error_name_length_required <- function() {
  "`n` must be specified, when the `names` attribute is `NULL`."
}

minimal_names <- function(name, n) {
  if (is.null(name) && missing(n)) {
    abort(error_name_length_required())
  }

  ## TODO: address scenarios where name is not NULL and n != length(name)?
  if (is.null(name)) {
    rep_len("", n)
  } else {
    name %|% ""
  }
}

set_minimal_names <- function(x) {
  new_names <- minimal_names(names(x), n = length(x))
  set_names(x, new_names)
}

unique_names <- function(name, quiet = FALSE, transform = identity) {
  min_name <- minimal_names(name)
  naked_name <- strip_pos(min_name)
  naked_is_empty <- (naked_name == "")

  new_name <- transform(naked_name)

  new_name <- append_pos(new_name, needs_suffix = naked_is_empty)

  duped_after <- duplicated(new_name) | duplicated(new_name, fromLast = TRUE)
  new_name <- append_pos(new_name, duped_after)

  if (!quiet) {
    describe_repair(name, new_name)
  }

  new_name
}

set_unique_names <- function(x, quiet = FALSE) {
  x <- set_minimal_names(x)
  new_names <- unique_names(names(x), quiet = quiet)
  set_names(x, new_names)
}

universal_names <- function(name, quiet = FALSE) {
  unique_names(name, quiet = quiet, transform = make_syntactic)
}

set_universal_names <- function(x, quiet = FALSE) {
  x <- set_minimal_names(x)
  new_names <- universal_names(names(x), quiet = quiet)
  set_names(x, new_names)
}

## makes each individual name syntactic
## does not enforce unique-ness
make_syntactic <- function(name) {
  name[is.na(name)]       <- ""
  name[name == ""]        <- "."
  name[name == "..."]     <- "...."
  name <- sub("^_", "._", name)

  new_name <- make.names(name)

  X_prefix <- grepl("^X", new_name) & !grepl("^X", name)
  new_name[X_prefix] <- sub("^X", "", new_name[X_prefix])

  dot_suffix <- which(new_name == paste0(name, "."))
  new_name[dot_suffix] <- sub("^(.*)[.]$", ".\\1", new_name[dot_suffix])
  ## illegal characters have been replaced with '.' via make.names()
  ## however, we have:
  ##   * declined its addition of 'X' prefixes
  ##   * turned its '.' suffixes to '.' prefixes

  regex <- paste0(
    "^(?<leading_dots>[.]{0,2})",
    "(?<numbers>[0-9]*)",
    "(?<leftovers>[^0-9]?.*$)"
  )

  re <- re_match(new_name, pattern = regex)
  needs_dots <- which(re$numbers != "")
  needs_third_dot <- (re$leftovers[needs_dots] == "")
  re$leading_dots[needs_dots] <- ifelse(needs_third_dot, "...", "..")
  new_name <- paste0(re$leading_dots, re$numbers, re$leftovers)

  new_name
}

append_pos <- function(name, needs_suffix) {
  need_append_pos <- which(needs_suffix)
  name[need_append_pos] <- paste0(name[need_append_pos], "..", need_append_pos)
  name
}

strip_pos <- function(name) {
  rx <- "[.][.][1-9][0-9]*$"
  gsub(rx, "", name) %|% ""
}

describe_repair <- function(orig_name, name) {
  if(length(orig_name) != length(name)) {
    abort(c(
      "`orig_name` and `name` have different sizes.",
      i = glue("`orig_name` is of size {length(orig_name)}."),
      i = glue("`name`      is of size {length(name)}.")
    ))
  }

  new_names <- name != minimal_names(orig_name)
  if (any(new_names)) {
    msg <- bullets(
      "New names:",
      paste0(
        tick_if_needed(orig_name[new_names]),
        " -> ",
        tick_if_needed(name[new_names]),
        .problem = ""
      )
    )
    message(msg)
  }
}

## from rematch2, except we don't add tbl_df or tbl classes to the return value
re_match <- function(text, pattern, perl = TRUE, ...) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern)) {
    abort(c(
      "incompatible `pattern`.",
      i = "`pattern` should be a scalar string."
    ))
  }
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  res
}

# A better version (with far more dependencies) exists in msg-format.R
bullets <- function(header, ..., .problem) {
  problems <- c(...)
  MAX_BULLETS <- 6L
  if (length(problems) >= MAX_BULLETS) {
    n_more <- length(problems) - MAX_BULLETS + 1L
    problems[[MAX_BULLETS]] <- "..."
    length(problems) <- MAX_BULLETS
  }

  paste0(
    header, "\n",
    paste0("* ", problems, collapse = "\n")
  )
}

# FIXME: Also exists in pillar, do we need to export?
tick <- function(x) {
  ifelse(is.na(x), "NA", encodeString(x, quote = "`"))
}

is_syntactic <- function(x) {
  ret <- (make_syntactic(x) == x)
  ret[is.na(x)] <- FALSE
  ret
}

tick_if_needed <- function(x) {
  needs_ticks <- !is_syntactic(x)
  x[needs_ticks] <- tick(x[needs_ticks])
  x
}
