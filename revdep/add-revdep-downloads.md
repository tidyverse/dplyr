add-revdep-downloads.R
================
jenny
2020-03-17

``` r
library(googlesheets4)
library(cranlogs)
library(tidyverse)
#> ── Attaching packages ──────────────────────────────────── tidyverse 1.2.1.9000 ──
#> ✓ ggplot2 3.2.1            ✓ purrr   0.3.3.9000  
#> ✓ tibble  2.99.99.9014     ✓ dplyr   0.8.99.9000 
#> ✓ tidyr   1.0.0.9000       ✓ stringr 1.4.0       
#> ✓ readr   1.3.1            ✓ forcats 0.4.0
#> ── Conflicts ──────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(here)
#> here() starts at /Users/jenny/rrr/dplyr

ssid <- "1m1u4f8PidRko2KGcXaz6BOYOcXREdr4Twc0zig9AEb8"
sheets_auth(email = "jenny@rstudio.com")
# sheets_browse(ssid)
dat <- sheets_read(ssid)
#> Reading from "dplyr 1.0.0 revdep checks"
#> Range "Sheet1"

dl_raw <- cran_downloads(when = "last-month", packages = dat$Package)

dl <- dl_raw %>%
  as_tibble() %>%
  count(package, wt = count) %>%
  arrange(package) %>%
  mutate(
    prop = n / sum(n),
    rank = min_rank(desc(n))
  )

# don't do this casually
#sheets_write(dl, ss = ssid, sheet = "downloads")

# NOTE: I then did assorted "hand work" in the actual Google Sheet:
#   * protected the 'downloads' sheet
#   * created 'downloads_n' named range as 'downloads!B2:B293'
#   * replaced static 'prop' column with '=B2/sum(downloads_n)'
#   * formatted 'prop' column as percentage with format string '0.0%'
#   * replaced static 'rank' column with '=rank(B2,downloads_n)'
#   * added 'dl_summary' column with
#     '=CONCATENATE(TO_TEXT(D2), " (", to_text(C2), ")")'
#   * created 'downloads_table' named range as 'downloads!A:E'
#   * inserted new column 'Downloads' into main table with
#     '=VLOOKUP(A3,downloads_table,5,FALSE)'
#   * protected the 'Downloads' column
#
# In hindsight, many of the formula columns above could have been created
# in R and included in the main write.

dl %>%
  arrange(desc(n)) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  print(n = 20)
#> # A tibble: 292 x 5
#>    package          n    prop  rank cum_prop
#>    <chr>        <dbl>   <dbl> <int>    <dbl>
#>  1 ggplot2    1277721 0.215       1    0.215
#>  2 tidyr       874486 0.147       2    0.362
#>  3 purrr       842567 0.142       3    0.503
#>  4 glue        796877 0.134       4    0.637
#>  5 broom       571030 0.0960      5    0.733
#>  6 dbplyr      373725 0.0628      6    0.796
#>  7 sf          316102 0.0531      7    0.849
#>  8 plotly      154538 0.0260      8    0.875
#>  9 recipes     122828 0.0206      9    0.896
#> 10 tidygraph    46927 0.00789    10    0.904
#> 11 janitor      32013 0.00538    11    0.909
#> 12 sjmisc       26743 0.00449    12    0.914
#> 13 DiagrammeR   26185 0.00440    13    0.918
#> 14 prophet      23095 0.00388    14    0.922
#> 15 padr         22353 0.00376    15    0.926
#> 16 mosaic       20333 0.00342    16    0.929
#> 17 tidyquant    19110 0.00321    17    0.932
#> 18 survminer    19004 0.00319    18    0.935
#> 19 ggformula    19002 0.00319    19    0.939
#> 20 skimr        18795 0.00316    20    0.942
#> # … with 272 more rows

write_csv(dl, here("revdep", "revep-downloads.csv"))
```
