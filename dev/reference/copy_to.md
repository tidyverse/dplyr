# Copy a local data frame to a remote src

This function uploads a local data frame into a remote data source,
creating the table definition as needed. Wherever possible, the new
object will be temporary, limited to the current connection to the
source.

## Usage

``` r
copy_to(dest, df, name = deparse(substitute(df)), overwrite = FALSE, ...)
```

## Arguments

- dest:

  remote data source

- df:

  local data frame

- name:

  name for new remote table.

- overwrite:

  If `TRUE`, will overwrite an existing table with name `name`. If
  `FALSE`, will throw an error if `name` already exists.

- ...:

  other parameters passed to methods.

## Value

a `tbl` object in the remote source

## Methods

This function is a **generic**, which means that packages can provide
implementations (methods) for other classes. See the documentation of
individual methods for extra arguments and differences in behaviour.

The following methods are currently available in loaded packages: dbplyr
([`src_sql`](https://dbplyr.tidyverse.org/reference/copy_to.src_sql.html)),
dplyr (`DBIConnection`) .

## See also

[`collect()`](https://dplyr.tidyverse.org/dev/reference/compute.md) for
the opposite action; downloading remote data into a local dbl.

## Examples

``` r
if (FALSE) { # \dontrun{
iris2 <- dbplyr::src_memdb() |> copy_to(iris, overwrite = TRUE)
iris2
} # }
```
