
lazy_vec_chop <- function(data, indices) {
  .Call(dplyr_lazy_vec_chop,
        new.env(parent = empty_env(), size = ncol(data), hash = TRUE),
        data, indices)
}
