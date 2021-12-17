# group_map() give meaningful errors

    Code
      (expect_error(mtcars %>% group_by(cyl) %>% group_modify(~ data.frame(cyl = 19)))
      )
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! The returned data frame cannot contain the original grouping variables: cyl.
    Code
      (expect_error(mtcars %>% group_by(cyl) %>% group_modify(~10)))
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! The result of `.f` must be a data frame.
    Code
      (expect_error(iris %>% group_by(Species) %>% group_modify(head1)))
    Output
      <error/rlang_error>
      Error in `group_modify()`:
      ! `.f` must accept at least two arguments.
      i You can use `...` to absorb unused components.
    Code
      (expect_error(iris %>% group_by(Species) %>% group_map(head1)))
    Output
      <error/rlang_error>
      Error in `group_map()`:
      ! `.f` must accept at least two arguments.
      i You can use `...` to absorb unused components.

