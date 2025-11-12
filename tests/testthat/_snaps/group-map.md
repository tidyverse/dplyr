# group_map() give meaningful errors

    Code
      (expect_error(group_modify(group_by(mtcars, cyl), ~ data.frame(cyl = 19))))
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! The returned data frame cannot contain the original grouping variables: cyl.
    Code
      (expect_error(group_modify(group_by(mtcars, cyl), ~10)))
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! The result of `.f` must be a data frame.
    Code
      (expect_error(group_modify(group_by(iris, Species), head1)))
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! `.f` must accept at least two arguments.
      i You can use `...` to absorb unused components.
    Code
      (expect_error(group_map(group_by(iris, Species), head1)))
    Output
      <error/rlang_error>
      Error in `group_map()`:
      ! `.f` must accept at least two arguments.
      i You can use `...` to absorb unused components.

# `keep =` is defunct

    Code
      group_map(df, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_map()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

---

    Code
      group_map(gdf, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_map()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

---

    Code
      group_modify(df, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_modify()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

---

    Code
      group_modify(gdf, keep = TRUE)
    Condition
      Error:
      ! The `keep` argument of `group_modify()` was deprecated in dplyr 1.0.0 and is now defunct.
      i Please use the `.keep` argument instead.

