# disk.frame

<details>

* Version: 0.3.7
* GitHub: https://github.com/xiaodaigh/disk.frame
* Source code: https://github.com/cran/disk.frame
* Date/Publication: 2020-07-07 13:10:03 UTC
* Number of recursive dependencies: 110

Run `cloud_details(, "disk.frame")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    count:
      function(x, ..., wt, sort, name)
    count.disk.frame:
      function(.data, ...)
    
    tally:
      function(x, wt, sort, name)
    tally.disk.frame:
      function(.data, ...)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    
    Found the following apparent S3 methods exported but not registered:
      count.disk.frame tally.disk.frame
    See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'select.disk.frame':
      ‘tally.disk.frame’ ‘count.disk.frame’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

# IRISMustangMetrics

<details>

* Version: 2.4.2
* GitHub: NA
* Source code: https://github.com/cran/IRISMustangMetrics
* Date/Publication: 2020-10-01 05:10:05 UTC
* Number of recursive dependencies: 31

Run `cloud_details(, "IRISMustangMetrics")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
    > ### Aliases: getMustangMetrics
    > ###   getMustangMetrics,IrisClient,character,character,character,character,POSIXct,POSIXct,character-method
    > ### Keywords: webservices
    > 
    > ### ** Examples
    > 
    > # Open a connection to IRIS DMC webservices (including the BSS)
    > iris <- new("IrisClient", debug=TRUE)
    > 
    > starttime <- as.POSIXct("2016-08-01", tz="GMT")
    > endtime <- starttime + 30*24*3600
    > metricName <- "sample_max,sample_mean,orientation_check"
    > 
    > # Get the measurement dataframe
    > juneStats <- getMustangMetrics(iris,"IU","ANMO","","BH[12Z]",
    +                                         starttime,endtime,metricName)
    URL = http://service.iris.edu/mustang/measurements/1/query?net=IU&sta=ANMO&cha=BH[12Z]&timewindow=2016-08-01T00:00:00,2016-08-31T00:00:00&format=text&metric=sample_max,sample_mean,orientation_check&nodata=404
    Error in getGeneralValueMetrics.IrisClient(obj, network, station, location,  : 
      getMustangMetrics.IrisClient: convert starttime  to as.POSIXct failed
    Calls: getMustangMetrics ... getMustangMetrics -> getGeneralValueMetrics.IrisClient
    Execution halted
    ```

# lplyr

<details>

* Version: 0.1.12
* GitHub: https://github.com/paulponcet/lplyr
* Source code: https://github.com/cran/lplyr
* Date/Publication: 2017-11-05 04:05:00 UTC
* Number of recursive dependencies: 60

Run `cloud_details(, "lplyr")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘lplyr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: mutate_.list
    > ### Title: Dplyr verbs for lists and pairlists
    > ### Aliases: mutate_.list mutate_.pairlist rename_.list rename_.pairlist
    > 
    > ### ** Examples
    > 
    > xs <- list(x1 = 1:3, 
    +            x2 = 2:5, 
    +            x3 = list("alpha", c("beta", "gamma")))
    > 
    > # Non-standard evaluation
    > mutate(xs, x4 = 4)
    Error in UseMethod("mutate") : 
      no applicable method for 'mutate' applied to an object of class "list"
    Calls: mutate
    Execution halted
    ```

# mosaicCore

<details>

* Version: 0.8.0
* GitHub: https://github.com/ProjectMOSAIC/mosaicCore
* Source code: https://github.com/cran/mosaicCore
* Date/Publication: 2020-09-13 14:10:03 UTC
* Number of recursive dependencies: 95

Run `cloud_details(, "mosaicCore")` for more info

</details>

## Newly broken

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Complete output:
      > library(testthat)
      > library(mosaicCore)
      > 
      > test_check("mosaicCore")
      Error: C stack usage  7976868 is too close to the limit
      Execution halted
    ```

# plotly

<details>

* Version: 4.9.2.1
* GitHub: https://github.com/ropensci/plotly
* Source code: https://github.com/cran/plotly
* Date/Publication: 2020-04-04 19:50:02 UTC
* Number of recursive dependencies: 153

Run `cloud_details(, "plotly")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     2 Abilene  2000     2    98  6505000  58700      746       6.6 2000.
     3 Abilene  2000     3   130  9285000  58100      784       6.8 2000.
     4 Abilene  2000     4    98  9730000  68600      785       6.9 2000.
     5 Abilene  2000     5   141 10590000  67300      794       6.8 2000.
     6 Abilene  2000     6   156 13910000  66900      780       6.6 2000.
     7 Abilene  2000     7   152 12635000  73500      742       6.2 2000.
     8 Abilene  2000     8   131 10710000  75000      765       6.4 2001.
     9 Abilene  2000     9   104  7615000  64500      771       6.5 2001.
    10 Abilene  2000    10   101  7040000  59300      764       6.6 2001.
    # … with 8,592 more rows
    > 
    > # dplyr verbs operate on plotly objects as if they were data frames
    > p <- economics %>%
    +   plot_ly(x = ~date, y = ~unemploy / pop) %>%
    +   add_lines() %>%
    +   mutate(rate = unemploy / pop) %>% 
    +   filter(rate == max(rate))
    Error in UseMethod("mutate") : 
      no applicable method for 'mutate' applied to an object of class "c('plotly', 'htmlwidget')"
    Calls: %>% -> filter -> mutate
    Execution halted
    ```

*   checking tests ... ERROR
    ```
      Running ‘testthat.R’
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      Warning (test-plotly-subplot.R:396:3): shape xref/yref bumping
      Warning (test-plotly-subplot.R:396:3): shape xref/yref bumping
      Warning (test-plotly-subplot.R:504:3): May specify legendgroup with through a vector of values
      Warning (test-plotly-symbol.R:30:3): Setting a constant symbol works
      Warning (test-plotly-symbol.R:68:3): Trace ordering is alphabetical
      Warning (test-plotly-symbol.R:68:3): Trace ordering is alphabetical
      Warning (test-plotly.R:41:3): Variable mappings return same result regardless of where they appear
      Warning (test-plotly.R:41:3): Variable mappings return same result regardless of where they appear
      Warning (test-plotly.R:143:3): Character strings correctly mapped to a positional axis
      Warning (test-plotly.R:143:3): Character strings correctly mapped to a positional axis
      ERROR (test-plotly.R:179:3): Complex example works
      
      [ FAIL 3 | WARN 65 | SKIP 54 | PASS 1363 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  6.8Mb
      sub-directories of 1Mb or more:
        htmlwidgets   3.7Mb
    ```

# tcR

<details>

* Version: 2.3.2
* GitHub: https://github.com/immunomind/immunarch
* Source code: https://github.com/cran/tcR
* Date/Publication: 2020-06-09 11:10:02 UTC
* Number of recursive dependencies: 76

Run `cloud_details(, "tcR")` for more info

</details>

## Newly broken

*   checking S3 generic/method consistency ... WARNING
    ```
    ...
      function(x, ..., wt, sort, name)
    count.frames:
      function(.data, .frame, .head, .coding)
    
    count:
      function(x, ..., wt, sort, name)
    count.inframes:
      function(.data, .head, .coding)
    
    count:
      function(x, ..., wt, sort, name)
    count.outframes:
      function(.data, .head)
    
    See section ‘Generic functions and methods’ in the ‘Writing R
    Extensions’ manual.
    
    Found the following apparent S3 methods exported but not registered:
      count.frames count.inframes count.outframes
    See section ‘Registering S3 methods’ in the ‘Writing R Extensions’
    manual.
    ```

*   checking Rd \usage sections ... NOTE
    ```
    S3 methods shown with full name in documentation object 'get.inframes':
      ‘count.inframes’ ‘count.outframes’ ‘count.frames’
    
    The \usage entries for S3 methods should use the \method markup and not
    their full name.
    See chapter ‘Writing R documentation files’ in the ‘Writing R
    Extensions’ manual.
    ```

## In both

*   checking installed package size ... NOTE
    ```
      installed size is  7.6Mb
      sub-directories of 1Mb or more:
        data   1.4Mb
        doc    3.8Mb
        libs   1.7Mb
    ```

# timetk

<details>

* Version: 2.6.0
* GitHub: https://github.com/business-science/timetk
* Source code: https://github.com/cran/timetk
* Date/Publication: 2020-11-21 14:10:02 UTC
* Number of recursive dependencies: 189

Run `cloud_details(, "timetk")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    ...
     7 D10   2014-07-09 2019.
     8 D10   2014-07-10 2007.
     9 D10   2014-07-11 2010 
    10 D10   2014-07-12 2002.
    # … with 10,133 more rows
    > 
    > # Remove Non-Working Days (Weekends & Holidays)
    > holidays <- tk_make_holiday_sequence(
    +     start_date = "2017-01-01",
    +     end_date   = "2017-12-31",
    +     calendar   = "NYSE")
    > 
    > FANG %>%
    +     group_by(symbol) %>%
    +     future_frame(.length_out       = "1 year",
    +                  .inspect_weekdays = TRUE,
    +                  .skip_values      = holidays)
    .date_var is missing. Using: date
    Error in is_quosure(x) : argument "x" is missing, with no default
    Calls: %>% ... switch_expr -> quo_squash_impl -> switch_expr -> is_quosure
    Execution halted
    ```

## In both

*   checking dependencies in R code ... NOTE
    ```
    Namespace in Imports field not imported from: ‘generics’
      All declared Imports should be used.
    ```

*   checking data for non-ASCII characters ... NOTE
    ```
      Note: found 2750 marked UTF-8 strings
    ```

