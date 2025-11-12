# Show warnings from the last command

Warnings that occur inside a dplyr verb like
[`mutate()`](https://dplyr.tidyverse.org/dev/reference/mutate.md) are
caught and stashed away instead of being emitted to the console. This
prevents rowwise and grouped data frames from flooding the console with
warnings. To see the original warnings, use `last_dplyr_warnings()`.

## Usage

``` r
last_dplyr_warnings(n = 5)
```

## Arguments

- n:

  Passed to [`head()`](https://rdrr.io/r/utils/head.html) so that only
  the first `n` warnings are displayed.
