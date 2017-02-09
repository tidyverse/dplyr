library(tidyverse)
#> Loading tidyverse: ggplot2
#> Loading tidyverse: tibble
#> Loading tidyverse: tidyr
#> Loading tidyverse: readr
#> Loading tidyverse: purrr
#> Loading tidyverse: dplyr
#> Conflicts with tidy packages ----------------------------------------------
#> filter(): dplyr, stats
#> lag():    dplyr, stats

con <- src_memdb()
copy_to(con, iris, "iris")
#> Source:   query [?? x 5]
#> Database: sqlite 3.11.1 [:memory:]
#>
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <dbl>       <dbl>        <dbl>       <dbl>   <chr>
#> 1           5.1         3.5          1.4         0.2  setosa
#> 2           4.9         3.0          1.4         0.2  setosa
#> 3           4.7         3.2          1.3         0.2  setosa
#> 4           4.6         3.1          1.5         0.2  setosa
#> 5           5.0         3.6          1.4         0.2  setosa
#> 6           5.4         3.9          1.7         0.4  setosa
#> 7           4.6         3.4          1.4         0.3  setosa
#> 8           5.0         3.4          1.5         0.2  setosa
#> 9           4.4         2.9          1.4         0.2  setosa
#> 10          4.9         3.1          1.5         0.1  setosa
#> # ... with more rows
iris_sql <- tbl(con, "iris")

iris_sql %>% do({
  head(.)
})
#> Error: length(select) not greater than 0L

iris_sql %>% group_by(Species) %>% do({
  head(.)
})
#> Source: local data frame [5 x 5]
#> Groups: Species [2]
#>
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#>          <dbl>       <dbl>        <dbl>       <dbl>     <chr>
#> 1          4.3         3.0          1.1         0.1    setosa
#> 2          4.4         2.9          1.4         0.2    setosa
#> 3          4.4         3.0          1.3         0.2    setosa
#> 4          4.4         3.2          1.3         0.2    setosa
#> 5          7.9         3.8          6.4         2.0 virginica

iris_sql %>% group_by(Species) %>% collect() %>% do({
  head(.)
})
#> Source: local data frame [18 x 5]
#> Groups: Species [3]
#>
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#>           <dbl>       <dbl>        <dbl>       <dbl>      <chr>
#> 1           5.1         3.5          1.4         0.2     setosa
#> 2           4.9         3.0          1.4         0.2     setosa
#> 3           4.7         3.2          1.3         0.2     setosa
#> 4           4.6         3.1          1.5         0.2     setosa
#> 5           5.0         3.6          1.4         0.2     setosa
#> 6           5.4         3.9          1.7         0.4     setosa
#> 7           7.0         3.2          4.7         1.4 versicolor
#> 8           6.4         3.2          4.5         1.5 versicolor
#> 9           6.9         3.1          4.9         1.5 versicolor
#> 10          5.5         2.3          4.0         1.3 versicolor
#> 11          6.5         2.8          4.6         1.5 versicolor
#> 12          5.7         2.8          4.5         1.3 versicolor
#> 13          6.3         3.3          6.0         2.5  virginica
#> 14          5.8         2.7          5.1         1.9  virginica
#> 15          7.1         3.0          5.9         2.1  virginica
#> 16          6.3         2.9          5.6         1.8  virginica
#> 17          6.5         3.0          5.8         2.2  virginica
#> 18          7.6         3.0          6.6         2.1  virginica

SpeciesList <- iris_sql %>% summarize(distinct(Species)) %>% collect() %>% .[["Species"]]
map_df(SpeciesList, function(name) {
  iris_sql %>% filter(Species == name) %>% collect() %>% head()
})
#> # A tibble: 18 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
#>           <dbl>       <dbl>        <dbl>       <dbl>      <chr>
#> 1           5.1         3.5          1.4         0.2     setosa
#> 2           4.9         3.0          1.4         0.2     setosa
#> 3           4.7         3.2          1.3         0.2     setosa
#> 4           4.6         3.1          1.5         0.2     setosa
#> 5           5.0         3.6          1.4         0.2     setosa
#> 6           5.4         3.9          1.7         0.4     setosa
#> 7           7.0         3.2          4.7         1.4 versicolor
#> 8           6.4         3.2          4.5         1.5 versicolor
#> 9           6.9         3.1          4.9         1.5 versicolor
#> 10          5.5         2.3          4.0         1.3 versicolor
#> 11          6.5         2.8          4.6         1.5 versicolor
#> 12          5.7         2.8          4.5         1.3 versicolor
#> 13          6.3         3.3          6.0         2.5  virginica
#> 14          5.8         2.7          5.1         1.9  virginica
#> 15          7.1         3.0          5.9         2.1  virginica
#> 16          6.3         2.9          5.6         1.8  virginica
#> 17          6.5         3.0          5.8         2.2  virginica
#> 18          7.6         3.0          6.6         2.1  virginica
