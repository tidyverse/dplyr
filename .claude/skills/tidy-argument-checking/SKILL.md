---
name: types-check
description: Validate function inputs in R using a standalone file of check_* functions. Use when writing exported R functions that need input validation, reviewing existing validation code, or when creating new input validation helpers.
---

# Input validation in R functions

This skill describes tidyverse style for validating function inputs. It focuses on rlang's exported type checkers along with the standalone file of `check_*` functions. These functions are carefully designed to produce clear, actionable error messages:

```r
check_string(123)
#> Error: `123` must be a single string, not the number 123.

check_number_whole(3.14, min = 1, max = 10)
#> Error: `3.14` must be a whole number, not the number 3.14.
```

It assumes that the user has already run `usethis::use_standalone("r-lib/rlang", "types-check")`, and imports rlang in their package. If not, confirm with the user before continuing.

## Function reference

### Scalars (single values)

Use scalar checkers when arguments parameterise the function (configuration flags, names, single counts), rather than represent vectors of user data. They all assert a single value.

- `check_bool()`: Single TRUE/FALSE (use for flags/options)
- `check_string()`: Single string (allows empty `""` by default)
- `check_name()`: Single non-empty string (for variable names, symbols as strings)
- `check_number_whole()`: Single integer-like numeric value
- `check_number_decimal()`: Single numeric value (allows decimals)

By default, scalar checkers do _not_ allow `NA` elements (`allow_na = FALSE`). Set `allow_na = TRUE` when missing values are allowed.

With the number checkers you can use `min` and `max` arguments for range validation, and `allow_infinite` (default `TRUE` for decimals, `FALSE` for whole numbers).

### Vectors

- `check_logical()`: Logical vector of any length
- `check_character()`: Character vector of any length
- `check_data_frame()`: A data frame object

By default, vector checkers allow `NA` elements (`allow_na = TRUE`). Set `allow_na = FALSE` when missing values are not allowed.

### Optional values: `allow_null`

Use `allow_null = TRUE` when `NULL` represents a valid "no value" state, similar to `Option<T>` in Rust or `T | null` in TypeScript:

```r
# NULL means "use default timeout"
check_number_decimal(timeout, allow_null = TRUE)
```

The tidyverse style guide recommends using `NULL` defaults instead of `missing()` defaults, so this pattern comes up often in practice.

## Other helpers

These functions are exported by rlang.

- `arg_match()`: Validates enumerated choices. Use when an argument must be one of a known set of strings.

  ```r
  # Validates and returns the matched value
  my_plot <- function(color = c("red", "green", "blue")) {
  color <- rlang::arg_match(color)
  # ...
  }

  my_plot("redd")
  #> Error in `my_plot()`:
  #> ! `color` must be one of "red", "green", or "blue", not "redd".
  #> ℹ Did you mean "red"?
  ```

  Note that partial matching is an error, unlike `base::match.arg()`. 

- `check_exclusive()` ensures only one of two arguments can be supplied. Supplying both together (i.e. both of them are non-`NULL`) is an error. Use `.require = FALSE` if both can be omitted.

- `check_required()`: Nice error message if required argument (i.e. can't be `missing()`) is not supplied.

## `call` and `arg` arguments

All check functions have `call` and `arg` arguments, but you should never use these unless you are creating your own `check_` function (see below for more details).

## When to validate inputs

**Validate at entry points, not everywhere.**

Input validation should happen at the boundary between user code and your package's internal implementation:

- **Exported functions**: Functions users call directly
- **Functions accepting user data**: Even internal functions if they directly consume user input, or external data (e.g. unserialised data)

Once inputs are validated at these entry points, internal helper functions can trust the data they receive without checking again.

A good analogy to keep in mind is gradual typing. Think of input validation like TypeScript type guards. Once you've validated data at the boundary, you can treat it as "typed" within your internal functions. Additional runtime checks are not needed. The entry point validates once, and all downstream code benefits.

Exception: Validate when in doubt. Do validate in internal functions if:
- The cost of invalid data is high (data corruption, security issues)
- The function or context is complex and you want defensive checks

Example of validating arguments of an exported function:

```r
# Exported function: VALIDATE
#' @export
create_report <- function(title, n_rows) {
  check_string(title)
  check_number_whole(n_rows, min = 1)

  # Now call helpers with validated data
  data <- generate_data(n_rows)
  format_report(title, data)
}
```

Once data is validated at the entry point, internal helpers can skip validation:

```r
# Internal helper: NO VALIDATION NEEDED
generate_data <- function(n_rows) {
  # n_rows is already validated, just use it
  data.frame(
    id = seq_len(n_rows),
    value = rnorm(n_rows)
  )
}

# Internal helper: NO VALIDATION NEEDED
format_report <- function(title, data) {
  # title and data are already validated, just use them
  list(
    title = title,
    summary = summary(data),
    rows = nrow(data)
  )
}
```

Note how the `data` generated by `generate_data()` doesn't need validation either. Internal code creating data in a trusted way (e.g. because it's simple or because it's covered by unit tests) doesn't require internal checks.

## Early input checking

Always validate inputs at the start of user-facing functions, before doing any work:

```r
my_function <- function(x, name, env = caller_env()) {
  check_logical(x)
  check_name(name)
  check_environment(env)

  # ... function body
}
```

Benefits:

- This self-documents the types of the arguments
- Eager evaluation also reduces the risk of confusing lazy evaluation effects

## Custom validation functions

Most packages will need one or more unique checker functions. Sometimes it's sufficient to wrap existing check functions with custom arguments. In this case you just need to carefully pass through the `arg` and `call` arguments. In other cases, you want a completely new check in which case you can call `stop_input_type` with your own arguments.

### Wrapping existing `check_` functions

When creating a wrapper or helper function that calls `check_*` functions on behalf of another function, you **must** propagate the caller context. Otherwise, errors will point to your wrapper function instead of the actual entry point.

Without proper propagation, error messages show the wrong function and argument names:

```r
# WRONG: errors will point to check_positive's definition
check_positive <- function(x) {
  check_number_whole(x, min = 1)
}

my_function <- function(count) {
  check_positive(count)
}

my_function(-5)
#> Error in `check_positive()`:  # Wrong! Should say `my_function()`
#> ! `x` must be a whole number larger than or equal to 1.  # Wrong! Should say `count`
```

With proper propagation, errors correctly identify the entry point and argument:

```r
# CORRECT: propagates context from the entry point
check_positive <- function(x, arg = caller_arg(x), call = caller_env()) {
  check_number_whole(x, min = 1, arg = arg, call = call)
}

my_function <- function(count) {
  check_positive(count)
}

my_function(-5)
#> Error in `my_function()`:  # Correct!
#> ! `count` must be a whole number larger than or equal to 1.  # Correct!
```

Note how `arg` and `call` are part of the function signature. That allows them to be wrapped again by another checking function that can pass down its own context.

### Creating a new `check_` function

When constructing your own `check_` function you can call `stop_input_type()` to take advantage of the existing infrastructure for generating error messages.
For example, imagine we wanted to create a function that checked that the input was a single date:

```R
check_date <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (!missing(x) && is.Date(x) && length(x) == 1) {
    return(invisible())
  }

  stop_input_type(
    x,
    "a single Date",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
```

Note you must always check first that the input is not missing, as `stop_input_type()` handles this case specially.

Sometimes you want to check if something is a compound type:

```R
check_string_or_bool <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
  if (!missing(x)) {
    if (is_string(x) || isTRUE(x) || isFALSE(x)) {
      return(invisible())
    }
  }

  stop_input_type(
    x, 
    c("a string", "TRUE", "FALSE"),
    ...,
    arg = arg,
    call = call
  )
}
```

Note that the second argument to `stop_input_type()` can take a vector, and it will automatically places commas and "and" in the appropriate locations.

Generally, you should place this `check_` function close to the function that is usually used to construct the object being checked (e.g. close to the S3/S4/S7 constructor.)
