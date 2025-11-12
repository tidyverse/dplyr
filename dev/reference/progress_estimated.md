# Progress bar with estimated time.

**\[deprecated\]**

This progress bar has been deprecated since providing progress bars is
not the responsibility of dplyr. Instead, you might try the more
powerful [progress](https://github.com/r-lib/progress) package.

This reference class represents a text progress bar displayed estimated
time remaining. When finished, it displays the total duration. The
automatic progress bar can be disabled by setting option
`dplyr.show_progress` to `FALSE`.

## Usage

``` r
progress_estimated(n, min_time = 0)
```

## Arguments

- n:

  Total number of items

- min_time:

  Progress bar will wait until at least `min_time` seconds have elapsed
  before displaying any results.

## Value

A ref class with methods `tick()`,
[`print()`](https://rdrr.io/r/base/print.html), `pause()`, and
[`stop()`](https://rdrr.io/r/base/stop.html).

## Examples

``` r
p <- progress_estimated(3)
#> Warning: `progress_estimated()` was deprecated in dplyr 1.0.0.
p$tick()
p$tick()
p$tick()

p <- progress_estimated(3)
#> Warning: `progress_estimated()` was deprecated in dplyr 1.0.0.
for (i in 1:3) p$pause(0.1)$tick()$print()

p <- progress_estimated(3)
#> Warning: `progress_estimated()` was deprecated in dplyr 1.0.0.
p$tick()$print()$
 pause(1)$stop()

# If min_time is set, progress bar not shown until that many
# seconds have elapsed
p <- progress_estimated(3, min_time = 3)
#> Warning: `progress_estimated()` was deprecated in dplyr 1.0.0.
for (i in 1:3) p$pause(0.1)$tick()$print()

if (FALSE) { # \dontrun{
p <- progress_estimated(10, min_time = 3)
for (i in 1:10) p$pause(0.5)$tick()$print()
} # }
```
