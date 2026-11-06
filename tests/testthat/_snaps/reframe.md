# `reframe()` throws intelligent recycling errors

    Code
      reframe(df, x = 1:2, y = 3:5)
    Condition
      Error in `reframe()`:
      ! Can't recycle `y = 3:5`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

---

    Code
      reframe(df, x = 1:2, y = 3:5, .by = g)
    Condition
      Error in `reframe()`:
      ! Can't recycle `y = 3:5`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

---

    Code
      reframe(gdf, x = 1:2, y = 3:5)
    Condition
      Error in `reframe()`:
      ! Can't recycle `y = 3:5`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

# `reframe()` recycling errors name the right expression when `across()` expands (#7657)

    Code
      reframe(df, across(everything(), function(x) rep(letters[x], each = x)))
    Condition
      Error in `reframe()`:
      ! Can't recycle `across(everything(), function(x) rep(letters[x], each = x))`.
      Caused by error:
      ! `across(everything(), function(x) rep(letters[x], each = x))` must be size 3 or 1, not 12.
      i An earlier column had size 3.

---

    Code
      reframe(df, across(everything(), identity), z = 1:2)
    Condition
      Error in `reframe()`:
      ! Can't recycle `z = 1:2`.
      Caused by error:
      ! `z` must be size 3 or 1, not 2.
      i An earlier column had size 3.

# `reframe()` doesn't message about regrouping when multiple group columns are supplied

    Code
      out <- reframe(df, x = mean(x), .by = c(a, b))

---

    Code
      out <- reframe(gdf, x = mean(x))

# `reframe()` doesn't message about regrouping when >1 rows are returned per group

    Code
      out <- reframe(df, x = vec_rep_each(x, x), .by = g)

---

    Code
      out <- reframe(gdf, x = vec_rep_each(x, x))

# catches `.by` with grouped-df

    Code
      reframe(gdf, .by = x)
    Condition
      Error in `reframe()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      reframe(rdf, .by = x)
    Condition
      Error in `reframe()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

