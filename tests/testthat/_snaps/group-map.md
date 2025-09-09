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

