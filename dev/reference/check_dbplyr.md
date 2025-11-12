# dbplyr compatibility functions

In dplyr 0.7.0, a number of database and SQL functions moved from dplyr
to dbplyr. The generic functions stayed in dplyr (since there is no easy
way to conditionally import a generic from different packages), but many
other SQL and database helper functions moved. If you have written a
backend, these functions generate the code you need to work with both
dplyr 0.5.0 dplyr 0.7.0.

## Usage

``` r
check_dbplyr()

wrap_dbplyr_obj(obj_name)
```

## Examples

``` r
wrap_dbplyr_obj("build_sql")
#> build_sql <- function (obj_name) 
#> {
#>     if (utils::packageVersion("dplyr") > "0.5.0") {
#>         dplyr::check_dbplyr()
#>         dbplyr::build_sql(obj_name = obj_name)
#>     }
#>     else {
#>         dplyr::build_sql(obj_name = obj_name)
#>     }
#> }
wrap_dbplyr_obj("base_agg")
#> base_agg <- function () 
#> {
#>     if (utils::packageVersion("dplyr") > "0.5.0") {
#>         dplyr::check_dbplyr()
#>         dbplyr::base_agg
#>     }
#>     else {
#>         dplyr::base_agg
#>     }
#> }
```
