#' Do anything
#'
#' \Sexpr[results=rd, stage=render]{dplyr:::lifecycle("questioning")}
#'
#' `do()` is marked as questioning as of dplyr 0.8.0, and may be advantageously
#' replaced by [group_modify()].
#'
#' @description This is a general purpose complement to the specialised
#' manipulation functions [filter()], [select()], [mutate()],
#' [summarise()] and [arrange()]. You can use `do()`
#' to perform arbitrary computation, returning either a data frame or
#' arbitrary objects which will be stored in a list. This is particularly
#' useful when working with models: you can fit models per group with
#' `do()` and then flexibly extract components with either another
#' `do()` or `summarise()`.
#'
#' For an empty data frame, the expressions will be evaluated once, even in the
#' presence of a grouping.  This makes sure that the format of the resulting
#' data frame is the same for both empty and non-empty input.
#'
#' @section Connection to plyr:
#'
#' If you're familiar with plyr, `do()` with named arguments is basically
#' equivalent to [plyr::dlply()], and `do()` with a single unnamed argument
#' is basically equivalent to [plyr::ldply()]. However, instead of storing
#' labels in a separate attribute, the result is always a data frame. This
#' means that `summarise()` applied to the result of `do()` can
#' act like `ldply()`.
#'
#' @param .data a tbl
#' @param ... Expressions to apply to each group. If named, results will be
#'   stored in a new column. If unnamed, should return a data frame. You can
#'   use `.` to refer to the current group. You can not mix named and
#'   unnamed arguments.
#' @return
#' `do()` always returns a data frame. The first columns in the data frame
#' will be the labels, the others will be computed from `...`. Named
#' arguments become list-columns, with one element for each group; unnamed
#' elements must be data frames and labels will be duplicated accordingly.
#'
#' Groups are preserved for a single unnamed input. This is different to
#' [summarise()] because `do()` generally does not reduce the
#' complexity of the data, it just expresses it in a special way. For
#' multiple named inputs, the output is grouped by row with
#' [rowwise()]. This allows other verbs to work in an intuitive
#' way.
#' @export
#' @examples
#' by_cyl <- group_by(mtcars, cyl)
#' do(by_cyl, head(., 2))
#'
#' models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .))
#' models
#'
#' summarise(models, rsq = summary(mod)$r.squared)
#' models %>% do(data.frame(coef = coef(.$mod)))
#' models %>% do(data.frame(
#'   var = names(coef(.$mod)),
#'   coef(summary(.$mod)))
#' )
#'
#' models <- by_cyl %>% do(
#'   mod_linear = lm(mpg ~ disp, data = .),
#'   mod_quad = lm(mpg ~ poly(disp, 2), data = .)
#' )
#' models
#' compare <- models %>% do(aov = anova(.$mod_linear, .$mod_quad))
#' # compare %>% summarise(p.value = aov$`Pr(>F)`)
#'
#' if (require("nycflights13")) {
#' # You can use it to do any arbitrary computation, like fitting a linear
#' # model. Let's explore how carrier departure delays vary over the time
#' carriers <- group_by(flights, carrier)
#' group_size(carriers)
#'
#' mods <- do(carriers, mod = lm(arr_delay ~ dep_time, data = .))
#' mods %>% do(as.data.frame(coef(.$mod)))
#' mods %>% summarise(rsq = summary(mod)$r.squared)
#'
#' \dontrun{
#' # This longer example shows the progress bar in action
#' by_dest <- flights %>% group_by(dest) %>% filter(n() > 100)
#' library(mgcv)
#' by_dest %>% do(smooth = gam(arr_delay ~ s(dep_time) + month, data = .))
#' }
#' }
do <- function(.data, ...) {
  UseMethod("do")
}

#' @export
do.NULL <- function(.data, ...) {
  NULL
}

# Helper functions -------------------------------------------------------------

env_bind_do_pronouns <- function(env, data) {
  if (is_function(data)) {
    bind <- env_bind_active
  } else {
    bind <- env_bind
  }

  # Use `:=` for `.` to avoid partial matching with `.env`
  bind(env, "." := data, .data = data)
}

label_output_dataframe <- function(labels, out, groups, .drop) {
  data_frame <- vapply(out[[1]], is.data.frame, logical(1))
  if (any(!data_frame)) {
    bad("Results {bad} must be data frames, not {first_bad_class}",
      bad = fmt_comma(which(!data_frame)),
      first_bad_class = fmt_classes(out[[1]][[which.min(data_frame)]])
    )
  }

  rows <- vapply(out[[1]], nrow, numeric(1))
  out <- bind_rows(out[[1]])

  if (!is.null(labels)) {
    # Remove any common columns from labels
    labels <- labels[setdiff(names(labels), names(out))]

    # Repeat each row to match data
    labels <- labels[rep(seq_len(nrow(labels)), rows), , drop = FALSE]
    rownames(labels) <- NULL

    grouped_df(bind_cols(labels, out), groups, .drop)
  } else {
    rowwise(out)
  }
}

label_output_list <- function(labels, out, groups) {
  if (!is.null(labels)) {
    labels[names(out)] <- out
    rowwise(labels)
  } else {
    class(out) <- "data.frame"
    attr(out, "row.names") <- .set_row_names(length(out[[1]]))
    rowwise(out)
  }
}

named_args <- function(args) {
  # Arguments must either be all named or all unnamed.
  named <- sum(names2(args) != "")
  if (!(named == 0 || named == length(args))) {
    abort("Arguments must either be all named or all unnamed")
  }
  if (named == 0 && length(args) > 1) {
    bad("Can only supply one unnamed argument, not {length(args)}")
  }

  # Check for old syntax
  if (named == 1 && names(args) == ".f") {
    abort("do syntax changed in dplyr 0.2. Please see documentation for details")
  }

  named != 0
}
