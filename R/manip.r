#' @examples
#'
#' data("baseball", package = "plyr")
#' bdf <- data_frame_source(baseball)
#' filter(bdf, year > 1980)
#' summarise(bdf, g = mean(g))
#' mutate(bdf, cyear = year - min(year) + 1)
#' arrange(bdf, id, desc(year))

filter <- function(`_data`, ...) UseMethod("filter")

filter.data.frame <- function(`_data`, ...) {
  nm <- deparse(substitute(`_data`))
  as.data.frame(filter(data_frame_source(`_data`, nm), ...))
}

filter.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)

  `_data`$filter <- c(`_data`$filter, args)
  `_data`
}

summarise <- function(`_data`, ...) UseMethod("summarise")

summarise.source <- function(`_data`, ..., `_env` = parent.frame()) {
  if (!is.null(`_data`$mutate)) {
    stop("Only one of summarise and mutate can be used",
      call. = FALSE)
  }

  args <- lapply(named_dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$summarise <- c(`_data`$summarise, args)
  `_data`
}

mutate <- function(`_data`, ...) UseMethod("mutate")

mutate.source <- function(`_data`, ..., `_env` = parent.frame()) {
  if (!is.null(`_data`$summarise)) {
    stop("Only one of summarise and mutate can be used",
      call. = FALSE)
  }
  args <- lapply(named_dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$mutate <- c(`_data`$mutate, args)
  `_data`
}

arrange <- function(`_data`, ...) UseMethod("arrange")
arrange.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)
  `_data`$arrange <- c(`_data`$arrange, args)
  `_data`
}

select <- function(`_data`, ...) UseMethod("select")
select.source <- function(`_data`, ..., `_env` = parent.frame()) {
  args <- lapply(dots(...), partial_eval, source = `_data`, env = `_env`)

  `_data`$select <- c(`_data`$select, args)
  `_data`
}

limit <- function(`_data`, n) UseMethod("limit")
limit.source <- function(`_data`, n) {
  `_data`$limit <- n
  `_data`
}
