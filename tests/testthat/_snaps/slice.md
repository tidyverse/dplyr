# slice errors if positive and negative indices mixed

    Code
      slice(tibble(), 1, -1)
    Condition
      Error in `slice()`:
      ! Can't compute indices.
      Caused by error:
      ! Must subset elements with a valid subscript vector.
      x Negative and positive locations can't be mixed.
      i Subscript `1` has a positive value at location 1.

# slicing with one-column matrix is deprecated

    Code
      out <- slice(df, matrix(c(1, 3)))
    Condition
      Warning:
      Slicing with a 1-column matrix was deprecated in dplyr 1.1.0.

# slice errors if index is not numeric

    Code
      slice(tibble(), "a")
    Condition
      Error in `slice()`:
      i In argument: `..1 = "a"`.
      Caused by error:
      ! Must subset elements with a valid subscript vector.
      x Subscript `"a"` has the wrong type `character`.
      i It must be numeric.

# user errors are correctly labelled

    Code
      slice(df, 1 + "")
    Condition
      Error in `slice()`:
      i In argument: `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(group_by(df, x), 1 + "")
    Condition
      Error in `slice()`:
      i In argument: `..1 = 1 + ""`.
      i In group 1: `x = 1`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# slice_helpers() call get_slice_size()

    Code
      slice_head(df, n = "a")
    Condition
      Error in `slice_head()`:
      ! `n` must be a round number, not a string.
    Code
      slice_tail(df, n = "a")
    Condition
      Error in `slice_tail()`:
      ! `n` must be a round number, not a string.
    Code
      slice_min(df, x, n = "a")
    Condition
      Error in `slice_min()`:
      ! `n` must be a round number, not a string.
    Code
      slice_max(df, x, n = "a")
    Condition
      Error in `slice_max()`:
      ! `n` must be a round number, not a string.
    Code
      slice_sample(df, n = "a")
    Condition
      Error in `slice_sample()`:
      ! `n` must be a round number, not a string.

# get_slice_size() validates its inputs

    Code
      get_slice_size(n = 1, prop = 1)
    Condition
      Error:
      ! Must supply `n` or `prop`, but not both.
    Code
      get_slice_size(n = foo)
    Condition
      Error:
      ! `n` must be a constant.
      Caused by error in `force()`:
      ! object 'foo' not found
    Code
      get_slice_size(prop = foo)
    Condition
      Error:
      ! `prop` must be a constant.
      Caused by error in `force()`:
      ! object 'foo' not found
    Code
      get_slice_size(n = "a")
    Condition
      Error:
      ! `n` must be a round number, not a string.
    Code
      get_slice_size(prop = "a")
    Condition
      Error:
      ! `prop` must be a number, not a string.

# n must be an integer

    Code
      slice_head(df, n = 1.1)
    Condition
      Error in `slice_head()`:
      ! `n` must be a round number, not a number.

# slice_*() checks that `n=` is explicitly named and ... is empty

    Code
      slice_head(df, 5)
    Condition
      Error in `slice_head()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_head(n = 5)`?
    Code
      slice_tail(df, 5)
    Condition
      Error in `slice_tail()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_tail(n = 5)`?
    Code
      slice_min(df, x, 5)
    Condition
      Error in `slice_min()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_min(n = 5)`?
    Code
      slice_max(df, x, 5)
    Condition
      Error in `slice_max()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_max(n = 5)`?
    Code
      slice_sample(df, 5)
    Condition
      Error in `slice_sample()`:
      ! `n` must be explicitly named.
      i Did you mean `slice_sample(n = 5)`?

---

    Code
      slice_head(df, 5, 2)
    Condition
      Error in `slice_head()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_tail(df, 5, 2)
    Condition
      Error in `slice_tail()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_min(df, x, 5, 2)
    Condition
      Error in `slice_min()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_max(df, x, 5, 2)
    Condition
      Error in `slice_max()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      slice_sample(df, 5, 2)
    Condition
      Error in `slice_sample()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 5
      * ..2 = 2
      i Did you forget to name an argument?

# slice_min/max() check size of `order_by=` (#5922)

    Code
      slice_min(data.frame(x = 1:10), 1:6)
    Condition
      Error in `slice_min()`:
      ! Can't compute indices.
      Caused by error:
      ! `order_by` must have size 10, not size 6.
    Code
      slice_max(data.frame(x = 1:10), 1:6)
    Condition
      Error in `slice_max()`:
      ! Can't compute indices.
      Caused by error:
      ! `order_by` must have size 10, not size 6.

# slice_min/max() validate simple arguments

    Code
      slice_min(data.frame(x = 1:10))
    Condition
      Error in `slice_min()`:
      ! `order_by` is absent but must be supplied.
    Code
      slice_max(data.frame(x = 1:10))
    Condition
      Error in `slice_max()`:
      ! `order_by` is absent but must be supplied.
    Code
      slice_min(data.frame(x = 1:10), x, with_ties = 1)
    Condition
      Error in `slice_min()`:
      ! `with_ties` must be `TRUE` or `FALSE`, not a number.
    Code
      slice_max(data.frame(x = 1:10), x, with_ties = 1)
    Condition
      Error in `slice_max()`:
      ! `with_ties` must be `TRUE` or `FALSE`, not a number.
    Code
      slice_min(data.frame(x = 1:10), x, na_rm = 1)
    Condition
      Error in `slice_min()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not a number.
    Code
      slice_max(data.frame(x = 1:10), x, na_rm = 1)
    Condition
      Error in `slice_max()`:
      ! `na_rm` must be `TRUE` or `FALSE`, not a number.

# slice_sample() checks size of `weight_by=` (#5922)

    Code
      slice_sample(df, n = 2, weight_by = 1:6)
    Condition
      Error in `slice_sample()`:
      ! Can't compute indices.
      Caused by error:
      ! `weight_by` must have size 10, not size 6.

# `slice_sample()` validates `replace`

    Code
      slice_sample(df, replace = 1)
    Condition
      Error in `slice_sample()`:
      ! `replace` must be `TRUE` or `FALSE`, not a number.
    Code
      slice_sample(df, replace = NA)
    Condition
      Error in `slice_sample()`:
      ! `replace` must be `TRUE` or `FALSE`, not `NA`.

