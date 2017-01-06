library(dplyr)
library(purrr)

df <- tibble(x = list(
  tibble(y = 1:2),
  tibble(y = 1:3),
  tibble(y = 1:4)
))

nrows <- function(df) {
  df %>% summarise(n = n()) %>% .[["n"]]
}

df %>%
  mutate(
    n1 = x %>% map_int(nrows),
    n2 = x %>% map_int(. %>% summarise(n = n()) %>% .[["n"]])
  )
#> # A tibble: 3 × 3
#>                  x    n1    n2
#>             <list> <int> <int>
#> 1 <tibble [2 × 1]>     2     3
#> 2 <tibble [3 × 1]>     3     3
#> 3 <tibble [4 × 1]>     4     3
