
#' Bind multiple data frames by column
#'
#' @description
#' Bind any number of data frames by column, making a wider result.
#' This is similar to `do.call(cbind, dfs)`.
#'
#' Where possible prefer using a [join][left_join] to combine multiple
#' data frames. `bind_cols()` binds the rows in order in which they appear
#' so it is easy to create meaningless results without realising it.
#'
#' @param ... Data frames to combine. Each argument can either be a data frame,
#'   a list that could be a data frame, or a list of data frames.
#'   Inputs are [recycled][vctrs::theory-faq-recycling] to the same length,
#'   then matched by position.
#' @param .name_repair One of `"unique"`, `"universal"`, or
#'   `"check_unique"`. See [vctrs::vec_as_names()] for the meaning of these
#'   options.
#' @returns A data frame the same type as the first element of `...`.
#' @export
#' @examples
#' df1 <- tibble(x = 1:3)
#' df2 <- tibble(y = 3:1)
#' bind_cols(df1, df2)
#'
#' # Row sizes must be compatible when column-binding
#' try(bind_cols(tibble(x = 1:3), tibble(y = 1:2)))
bind_cols <- function(..., .name_repair = c("unique", "universal", "check_unique", "minimal")) {
  dots <- list2(...)

  dots <- list_flatten(dots, recursive = TRUE)
  dots <- discard(dots, is.null)

  # Strip names off of data frame components so that vec_cbind() unpacks them
  names2(dots)[map_lgl(dots, is.data.frame)] <- ""

  out <- vec_cbind(!!!dots, .name_repair = .name_repair, .error_call = current_env())
  if (!any(map_lgl(dots, is.data.frame))) {
    out <- as_tibble(out)
  }
  if (length(dots) && is.data.frame(first <- dots[[1L]])) {
    out <- dplyr_reconstruct(out, first)
  }
  out
}
