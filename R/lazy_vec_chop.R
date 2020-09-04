
dplyr_lazy_vec_chop <- function(data) {
  .Call(dplyr_lazy_vec_chop_impl, data)
}

resolved <- function(env) {
  .Call(env_resolved, env, env_names(env))
}

dplyr_data_masks <- function(chops, data) {
  masks <- .Call(dplyr_data_masks_setup, chops, data)

  for (i in seq_along(masks)) {
    masks[[i]] <- new_data_mask(masks[[i]])
    masks[[i]]$.data <- as_data_pronoun(masks[[i]])
  }
  masks
}
