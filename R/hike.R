#' Run expression in each group
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param .data A data frame
#' @param expr An expression to execute for each group
#'
#' @return
#' - [hike] returns the results in a list
#' - [march] discards the results and returns `.data` invisibly
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   march(
#'     print(head(across(), 2))
#'   )
#'
#' iris %>%
#'   group_by(Species) %>%
#'   hike(
#'     broom::tidy(lm(Petal.Length ~ Sepal.Length))
#'   )
#'
#' @export
hike <- function(.data, expr) {
  UseMethod("hike")
}

#' @export
hike.data.frame <- function(.data, expr) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }
  mask <- DataMask$new(.data, caller_env(), rows)
  mask$eval_all(enquo(expr))
}

#' @rdname hike
#' @export
march <- function(.data, expr) {
  UseMethod("march")
}

#' @export
march.data.frame <- function(.data, expr) {
  hike(.data, {{expr}})
  invisible(.data)
}
