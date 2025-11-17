# group_split.grouped_df() warns about `...`

    Code
      out <- group_split(group_by(mtcars, cyl), cyl)
    Condition
      Warning:
      Calling `group_split()` on a <grouped_df> ignores `...`. Please use `group_by(..., .add = TRUE) |> group_split()`.

# group_split.rowwise_df() warns about `...`

    Code
      out <- group_split(rowwise(mtcars), cyl)
    Condition
      Warning:
      Calling `group_split()` on a <rowwise_df> ignores `...`. Please use `as_tibble() |> group_split(...)`.

# `keep =` is defunct

    Code
      group_split(df, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_split()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

---

    Code
      group_split(gdf, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_split()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

---

    Code
      group_split(rdf, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_split()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

