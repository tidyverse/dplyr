# Articles

### Get started

- [Introduction to
  dplyr](https://dplyr.tidyverse.org/dev/articles/dplyr.md):

  Start here if this is your first time using dplyr. You’ll learn the
  basic philosophy, the most important data manipulation verbs, and the
  pipe, `|>`, which allows you to combine multiple verbs together to
  solve real problems.

- [Grouped data](https://dplyr.tidyverse.org/dev/articles/grouping.md):

  To unlock the full potential of dplyr, you need to understand how each
  verb interacts with grouping. This vignette shows you how to
  manipulate grouping, how each verb changes its behaviour when working
  with grouped data, and how you can access data about the “current”
  group from within a verb.

- [Two-table
  verbs](https://dplyr.tidyverse.org/dev/articles/two-table.md):

  Most dplyr verbs work with a single data set, but most data analyses
  involve multiple datasets. This vignette introduces you to the dplyr
  verbs that work with more one than data set, and introduces to the
  mutating joins, filtering joins, and the set operations.

- [dplyr \<-\> base
  R](https://dplyr.tidyverse.org/dev/articles/base.md):

  How does dplyr compare to base R? This vignette describes the main
  differences in philosophy, and shows the base R code most closely
  equivalent to each dplyr verb.

### Automate

- [Column-wise
  operations](https://dplyr.tidyverse.org/dev/articles/colwise.md):

  Learn how to easily repeat the same operation across multiple columns
  using
  [`across()`](https://dplyr.tidyverse.org/dev/reference/across.md).

- [Row-wise
  operations](https://dplyr.tidyverse.org/dev/articles/rowwise.md):

  In R, it’s usually easier to do something for each column than for
  each row. In this vignette you will learn how to use the
  [`rowwise()`](https://dplyr.tidyverse.org/dev/reference/rowwise.md)
  function to perform operations by row. Along the way, you’ll learn
  about list-columns, and see how you might perform simulations and
  modelling within dplyr verbs.

- [Programming with
  dplyr](https://dplyr.tidyverse.org/dev/articles/programming.md):

  Most dplyr verbs use “tidy evaluation”, a special type of non-standard
  evaluation. In this vignette, you’ll learn the two basic forms, data
  masking and tidy selection, and how you can program with them using
  either functions or for loops.

### Other

- [Window
  functions](https://dplyr.tidyverse.org/dev/articles/window-functions.md):

  Window functions are a useful family of functions that work with
  vectors (returning an output the same size as the input), and combine
  naturally with
  [`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) and
  [`filter()`](https://dplyr.tidyverse.org/dev/reference/filter.md).

- [Using dplyr in
  packages](https://dplyr.tidyverse.org/dev/articles/in-packages.md):

  A guide for package authors who use dplyr.
