
dplyr_lazy_vec_chop <- function(data, rows) {
  .Call(dplyr_lazy_vec_chop_impl, data, rows)
}

dplyr_data_masks <- function(chops, data, rows) {
  masks <- .Call(dplyr_data_masks_setup, chops, data, rows)

  for (i in seq_along(masks)) {
    masks[[i]] <- new_data_mask(masks[[i]])
    masks[[i]]$.data <- as_data_pronoun(masks[[i]])
  }
  masks
}
