# can't explicitly select grouping columns (#5460)

    Code
      mutate(gdf, y = pick(g))
    Condition
      Error in `mutate()`:
      i In argument: `y = pick(g)`.
      i In group 1: `g = 1`.
      Caused by error in `pick()`:
      ! Can't subset columns that don't exist.
      x Column `g` doesn't exist.

# `all_of()` is evaluated in the correct environment (#5460)

    Code
      mutate(df, z = pick(all_of(y)))
    Condition
      Error in `mutate()`:
      i In argument: `z = pick(all_of(y))`.
      Caused by error in `pick()`:
      ! Problem while evaluating `all_of(y)`.
      Caused by error in `as_indices_impl()`:
      ! object 'y' not found

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

# requires at least one input

    Code
      mutate(data.frame(), pick())
    Condition
      Error in `mutate()`:
      i In argument: `..1 = pick()`.
      Caused by error in `pick()`:
      ! `...` can't be empty.

# doesn't allow renaming

    Code
      mutate(data.frame(x = 1), pick(y = x))
    Condition
      Error in `mutate()`:
      i In argument: `..1 = pick(y = x)`.
      Caused by error in `pick()`:
      ! Can't rename variables in this context.

