# group_nest.grouped_df() warns about `...`

    Code
      group_nest(group_by(mtcars, cyl), cyl)
    Condition
      Warning:
      Calling `group_nest()` on a <grouped_df> ignores `...`. Please use `group_by(..., .add = TRUE) |> group_nest()`.
    Output
      # A tibble: 3 x 2
          cyl                data
        <dbl> <list<tibble[,10]>>
      1     4           [11 x 10]
      2     6            [7 x 10]
      3     8           [14 x 10]

