# We avoid ncol() from base R because it calls dim(),
# and that would materialize an ALTREP duckplyr data frame.
ncol <- function(x) {
  length(x)
}
