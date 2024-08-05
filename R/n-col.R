# Masks `ncol()` to avoid accidentally materializing ALTREP duckplyr
# data frames.
ncol <- function(x) {
  abort("Use `df_n_col()` or `mat_n_col()` instead.")
}

# Alternative to `ncol()` which avoids `dim()`.
#
# `dim()` also requires knowing the number of rows,
# which forces ALTREP duckplyr data frames to materialize.
#
# This function makes the same assertion as vctrs about data frame structure,
# i.e. if `x` inherits from `"data.frame"`, then it is a VECSXP with length
# equal to the number of columns.
df_n_col <- function(x) {
  x <- unclass(x)
  obj_check_list(x)
  length(x)
}

# In a few places we call `ncol()` on matrices, and in those
# cases we want to continue using the base version.
mat_n_col <- function(x) {
  base::ncol(x)
}
