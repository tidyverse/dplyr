#' Argument type: data-masking
#'
#' @description
#' This page the describes the `<data-masking>` argument modifier which
#' indicates that the argument uses tidy evaluation with **data masking**.
#' If you've never head of tidy evaluation before, start with
#' `vignette("programming")`.
#'
#' # Key terms
#'
#' The primary motivation for tidy evaluation in dplyr is that it provides
#' **data masking**, which blurs the distinction between two types of variables:
#'
#' * __env-variables__ are "programming" variables and live in an environment.
#'   They are usually created with `<-`. Env-variables can be any type of R
#'   object.
#'
#' * __data-variables__ are "statistical" variables and live in a data frame.
#'   They usually come from data files (e.g. `.csv`, `.xls`), or are created by
#'   manipulating existing variables. Data-variables live inside data frames,
#'   so must be vectors.
#'
#' # General usage
#'
#' Data masking allows you to refer to variables in the "current" data frame
#' (usually supplied in the `.data` argument), without any other prefix.
#' It's what allows you to type (e.g.) `filter(diamonds, x == 0 & y == 0 & z == 0)`
#' instead of `diamonds[diamonds$x == 0 & diamonds$y == 0 & diamonds$z == 0, ]`.
#'
#' # Indirection
#'
#' The main challenge of data masking arises when you introduce some
#' indirection, i.e. instead of directly typing the name of a variable you
#' want to supply it in a function argument or character vector.
#'
#' There are two main cases:
#'
#' *  If you want the user to supply the variable (or function of variables)
#'    in a function argument, embrace the argument, e.g. `filter(df, {{ var }})`.
#'
#'    ```
#'    dist_summary <- function(df, var) {
#'      df %>%
#'        summarise(n = n(), min = min({{ var }}), max = max({{ var }}))
#'    }
#'    mtcars %>% dist_summary(mpg)
#'    mtcars %>% group_by(cyl) %>% dist_summary(mpg)
#'    ```
#'
#' *  If you have the column name as a character vector, use the `.data`
#'    pronoun, e.g. `summarise(df, mean = mean(.data[[var]]))`.
#'
#'    ```
#'    for (var in names(mtcars)) {
#'      mtcars %>% count(.data[[var]]) %>% print()
#'    }
#'
#'    lapply(names(mtcars), function(var) mtcars %>% count(.data[[var]]))
#'    ```
#'
#' # Dot-dot-dot (...)
#'
#' When this modifier is applied to `...`, there is one other useful technique
#' which solves the problem of creating a new variable with a name supplied
#' by the user. Use `!!var := expression`. (Note the use of `:=` instead of
#' `=`).
#'
#' ```
#' var_name <- "l100km"
#' mtcars %>% mutate(!!var_name := 235 / mpg)
#' ```
#'
#' Note that `...` automatically provides indirection, so you can use it as is
#' (i.e. without embracing) inside a function:
#'
#' ```
#' grouped_mean <- function(df, var, ...) {
#'   df %>%
#'     group_by(...) %>%
#'     summarise(mean = mean({{ var }}))
#' }
#' ```
#'
#' @keywords internal
#' @name dplyr_data_masking
NULL


#' Argument type: tidy-select
#'
#' @description
#' This page the describes the `<tidy-select>` argument modifier which indicates
#' the argument uses tidy evaluation with **tidy selection**. Tidy selection
#' provides a concise DSL for selecting variables based on their names.
#' If you've never head of tidy evaluation before, start with
#' `vignette("programming")`.
#'
#' # General usage
#'
#' If you have a data frame with variables `apple`, `banana`, `cantelope`,
#' `date`, `eggplant`, `fig`, `grape` you can:
#'
#' * Select individual variables with their name: e.g. `c(apple, fig, grape)`.
#'
#' * Select data-variables stored in a env-variable with `all_of()` (which
#'   will error if a variable is not found) or `any_of()` (which is
#'   relaxed and will silently drop missing variables), e.g.
#'   if `vars <- c("apple", "fig", "peach")`, then `all_of(vars)` will
#'   error; `any_of(vars)` will select `apple` and `fig`.
#'
#' * Select contiguous variables with `:`, e.g. `apple:date`.
#'
#' * Select variables with named-based helpers: e.g. `ends_with("a")`,
#'   `contains("g")`. See full list in [tidyselect::select_helpers].
#'
#' * Select variables of a given type with a `is` function: `is.numeric`,
#'   `is.factor`, `is.character`, etc.
#'
#' * Invert a selection with `!`: `!is.numeric`, or `!contains("x")`.
#'
#' * Create logical combination with `|` and `&`:
#'   `starts_with("a") | starts_with("b")`, `contains("x") & is.numeric`
#'
#' * Remove variables from a collection with `-`:
#'   `is.numeric - starts_with("a")`
#'
#' # Indirection
#'
#' There are two main cases:
#'
#' *   If you have a character vector of column names, use `all_of()`
#'     or `any_of()`, depending on whether or not you want unknown variable
#'     names to cause an error, e.g `select(df, all_of(vars))`,
#'     `select(df, -any_of(vars))`.
#'
#' *   If you you want the user to supply a tidyselect specification in a
#'     function argument, embrace the function argument, e.g
#'     `select(df, {{ vars }})`.
#'
#' @keywords internal
#' @name dplyr_tidy_select
NULL
