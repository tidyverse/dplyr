# throws tidyselect errors

    Code
      compute_by(by = y, data = df)
    Condition
      Error:
      ! Can't subset columns that don't exist.
      x Column `y` doesn't exist.

# can't set `.by` with a grouped-df

    Code
      compute_by(x, gdf)
    Condition
      Error:
      ! Can't supply `by` when `data` is a grouped data frame.

# can't set `.by` with a rowwise-df

    Code
      compute_by(x, rdf)
    Condition
      Error:
      ! Can't supply `by` when `data` is a rowwise data frame.

# can tweak the error args

    Code
      compute_by(x, gdf, by_arg = "x", data_arg = "dat")
    Condition
      Error:
      ! Can't supply `x` when `dat` is a grouped data frame.

