#' Standard Summary
#'
#' This function calculates standard summary statistics for specified variables in a data frame.
#'
#' @param df A data frame.
#' @param vars A character vector specifying the variables for which summary statistics should be calculated.
#' @param functions A list of functions to be applied to each variable. The default functions include mean, standard deviation, minimum, 10th percentile, 25th percentile, median, 75th percentile, 90th percentile, maximum, count, and count of missing values.
#'
#' @return A data frame containing the calculated summary statistics.
#'
#' @examples
#' df <- data.frame(
#'   groups = c("a","a","a","b","c")
#'   var1 = c(1, 2, 3, 4, 5),
#'   var2 = c(6, 7, NA, 9, 10)
#' )
#'
#' # Calculate standard summary statistics for var1 and var2
#' summary <- df %>% group_by(groups) %>% standard_summary(c("var1", "var2"))
#' print(summary)
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom stats mean sd min quantile max
#' @importFrom base length which is.na
#'
#' @export
standard_summary <- function(df, vars, functions = list(
  mean = ~mean(.x, na.rm = TRUE),
  sd = ~sd(.x, na.rm = TRUE),
  min = ~min(.x, na.rm = TRUE),
  q10 = ~quantile(.x, 0.1, na.rm = TRUE),
  q25 = ~quantile(.x, 0.25, na.rm = TRUE),
  med = ~quantile(.x, 0.5, na.rm = TRUE),
  q75 = ~quantile(.x, 0.75, na.rm = TRUE),
  q90 = ~quantile(.x, 0.90, na.rm = TRUE),
  max = ~max(.x, na.rm = TRUE),
  n = ~n(),
  nmiss = ~length(which(is.na(.x))))
) {
  gg <- as.character(groups(df))
  summary_res <- df %>%
    select(!!vars) %>%
    summarise(across(.cols = c(!!vars), .fns = functions, .names = "{.col}xx_xx{.fn}")) %>%
    ungroup() %>%
    pivot_longer(cols = contains("xx")) %>%
    separate(name, into = c("VARIABLE", "STAT"), sep = "xx_xx") %>%
    pivot_wider(id_cols = c(gg, "VARIABLE"), values_from = value, names_from = STAT)
}