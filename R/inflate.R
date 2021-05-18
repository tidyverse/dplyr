#' @export
inflate <- function(.data, ...) {
  UseMethod("inflate")
}

#' @export
inflate.data.frame <- function(.data, ...){
  cols <- mutate_cols(.data, ..., caller_env = caller_env())
  out <- dplyr_col_modify(.data, cols)

  # make sure sizes of the columns to unnest are compatible
  to_unnest <- names(cols)
  sizes <- map(out[to_unnest], list_sizes)
  if (length(cols) > 1) {
    for (i in seq2(2, length(cols))) {
      if (!identical(sizes[[1]], sizes[[i]])) {
        abort("incompatible sizes")
      }
    }
  }
  sizes <- sizes[[1]]

  # flatten the columns to unnest, recycle the others
  out <- as_list(out)
  out[to_unnest] <- map(out[to_unnest], unlist)
  out[setdiff(names(out), to_unnest)] <- map(out[setdiff(names(out), to_unnest)], vec_rep_each, sizes)
  out <- vctrs::new_data_frame(out)

  dplyr_reconstruct(out, .data)
}

