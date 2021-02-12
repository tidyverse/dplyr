# group_map() give meaningful errors

    Code
      mtcars %>% group_by(cyl) %>% group_modify(~data.frame(cyl = 19))
    Error <rlang_error>
      The returned data frame cannot contain the original grouping variables: cyl.

---

    Code
      mtcars %>% group_by(cyl) %>% group_modify(~10)
    Error <rlang_error>
      The result of .f should be a data frame.

---

    Code
      iris %>% group_by(Species) %>% group_modify(head1)
    Error <simpleError>
      The function must accept at least two arguments. You can use ... to absorb unused components

---

    Code
      iris %>% group_by(Species) %>% group_map(head1)
    Error <simpleError>
      The function must accept at least two arguments. You can use ... to absorb unused components

