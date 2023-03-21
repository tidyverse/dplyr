# with `rowwise()` data, leaves list-cols unwrapped (#5951, #6264)

    Code
      mutate(rdf, z = pick(x, y))
    Condition
      Error in `mutate()`:
      i In argument: `z = pick(x, y)`.
      i In row 2.
      Caused by error:
      ! `z` must be size 1, not 2.
      i Did you mean: `z = list(pick(x, y))` ?

---

    Code
      mutate(rdf, z = pick_wrapper(x, y))
    Condition
      Error in `mutate()`:
      i In argument: `z = pick_wrapper(x, y)`.
      i In row 2.
      Caused by error:
      ! `z` must be size 1, not 2.
      i Did you mean: `z = list(pick_wrapper(x, y))` ?

# can't explicitly select grouping columns (#5460)

    Code
      mutate(gdf, y = pick(g))
    Condition
      Error in `mutate()`:
      i In argument: `y = pick(g)`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `g` doesn't exist.

---

    Code
      mutate(gdf, y = pick_wrapper(g))
    Condition
      Error in `mutate()`:
      i In argument: `y = pick_wrapper(g)`.
      i In group 1: `g = 1`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `g` doesn't exist.

# must supply at least one selector to `pick()`

    Code
      mutate(df, y = pick())
    Condition
      Error in `mutate()`:
      i In argument: `y = pick()`.
      Caused by error in `pick()`:
      ! Must supply at least one input to `pick()`.

---

    Code
      mutate(df, y = pick_wrapper())
    Condition
      Error in `mutate()`:
      i In argument: `y = pick_wrapper()`.
      Caused by error in `pick()`:
      ! Must supply at least one input to `pick()`.

# the tidyselection and column extraction are evaluated on the current data

    Code
      mutate(gdf, x = NULL, y = pick(x))
    Condition
      Error in `mutate()`:
      i In argument: `y = pick(x)`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `x` doesn't exist.

---

    Code
      mutate(gdf, x = NULL, y = pick_wrapper(x))
    Condition
      Error in `mutate()`:
      i In argument: `y = pick_wrapper(x)`.
      i In group 1: `g = 1`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `x` doesn't exist.

# can call `pick()` from a user defined function

    Code
      mutate(gdf, d = my_pick())
    Condition
      Error in `mutate()`:
      i In argument: `d = my_pick()`.
      i In group 1: `a = 1`.
      Caused by error in `all_of()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

---

    Code
      mutate(gdf, d = my_pick(y))
    Condition
      Error in `mutate()`:
      i In argument: `d = my_pick(y)`.
      i In group 1: `a = 1`.
      Caused by error in `all_of()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# errors correctly outside mutate context

    Code
      pick()
    Condition
      Error in `pick()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

---

    Code
      pick(a, b)
    Condition
      Error in `pick()`:
      ! Must only be used inside data-masking verbs like `mutate()`, `filter()`, and `group_by()`.

# when expansion occurs, error labels use the pre-expansion quosure

    Code
      mutate(df, if (cur_group_id() == 1L) pick(x) else "x", .by = g)
    Condition
      Error in `mutate()`:
      i In argument: `if (cur_group_id() == 1L) pick(x) else "x"`.
      Caused by error:
      ! `if (cur_group_id() == 1L) pick(x) else "x"` must return compatible vectors across groups.
      i Result of type <tbl_df<x:double>> for group 1: `g = 1`.
      i Result of type <character> for group 2: `g = 2`.

# doesn't allow renaming

    Code
      mutate(data.frame(x = 1), pick(y = x))
    Condition
      Error in `mutate()`:
      i In argument: `pick(y = x)`.
      Caused by error in `pick()`:
      ! Can't rename variables in this context.

---

    Code
      mutate(data.frame(x = 1), pick_wrapper(y = x))
    Condition
      Error in `mutate()`:
      i In argument: `pick_wrapper(y = x)`.
      Caused by error in `pick()`:
      ! Can't rename variables in this context.

# `pick()` errors in `arrange()` are useful

    Code
      arrange(df, pick(y))
    Condition
      Error in `arrange()`:
      i In argument: `..1 = pick(y)`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.

---

    Code
      arrange(df, foo(pick(x)))
    Condition
      Error in `arrange()`:
      i In argument: `..1 = foo(pick(x))`.
      Caused by error in `foo()`:
      ! could not find function "foo"

# `filter()` with `pick()` that uses invalid tidy-selection errors

    Code
      filter(df, pick(x, a))
    Condition
      Error in `filter()`:
      i In argument: `pick(x, a)`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

---

    Code
      filter(df, pick_wrapper(x, a))
    Condition
      Error in `filter()`:
      i In argument: `pick_wrapper(x, a)`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `a` doesn't exist.

# `filter()` that doesn't use `pick()` result correctly errors

    Code
      filter(df, pick(x, y)$x)
    Condition
      Error in `filter()`:
      i In argument: `asNamespace("dplyr")$dplyr_pick_tibble(x = x, y = y)$x`.
      Caused by error:
      ! `..1` must be a logical vector, not a double vector.

---

    Code
      filter(df, pick_wrapper(x, y)$x)
    Condition
      Error in `filter()`:
      i In argument: `pick_wrapper(x, y)$x`.
      Caused by error:
      ! `..1` must be a logical vector, not a double vector.

