# `morph()` throws intelligent recycling errors

    Code
      morph(df, x = 1:2, y = 3:5)
    Condition
      Error in `morph()`:
      ! Can't recycle `y = 3:5`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

---

    Code
      morph(df, x = 1:2, y = 3:5, .by = g)
    Condition
      Error in `morph()`:
      ! Can't recycle `y = 3:5`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

---

    Code
      morph(gdf, x = 1:2, y = 3:5)
    Condition
      Error in `morph()`:
      ! Can't recycle `y = 3:5`.
      i In group 1: `g = 1`.
      Caused by error:
      ! `y` must be size 2 or 1, not 3.
      i An earlier column had size 2.

# `morph()` doesn't message about regrouping when multiple group columns are supplied

    Code
      out <- morph(df, x = mean(x), .by = c(a, b))

---

    Code
      out <- morph(gdf, x = mean(x))

# `morph()` doesn't message about regrouping when >1 rows are returned per group

    Code
      out <- morph(df, x = vec_rep_each(x, x), .by = g)

---

    Code
      out <- morph(gdf, x = vec_rep_each(x, x))

# catches `.by` with grouped-df

    Code
      morph(gdf, .by = x)
    Condition
      Error in `morph()`:
      ! Can't supply `.by` when `.data` is a grouped data frame.

# catches `.by` with rowwise-df

    Code
      morph(rdf, .by = x)
    Condition
      Error in `morph()`:
      ! Can't supply `.by` when `.data` is a rowwise data frame.

