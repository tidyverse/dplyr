#' Run expression in each group
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' @param .data A data frame
#' @param expr An expression to execute for each group
#'
#' @examples
#' iris %>%
#'   group_by(Species) %>%
#'   hike(
#'     mod = lm(Petal.Length ~ Sepal.Length, data = across()),
#'     print(broom::tidy(mod))
#'   )
#'
#' @export
hike <- function(.data, ...) {
  UseMethod("hike")
}

#' @export
hike.data.frame <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }
  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(..., .named = TRUE)
  dots_names <- names(dots)

  for (i in seq_along(quos)) {
    mask$add(dots_names[i], mask$eval_all(dots[[i]]))
  }

  invisible(.data)
}
