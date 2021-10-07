# do() gives meaningful error messages

    Code
      (expect_error(df %>% do(head, tail)))
    Output
      <error/rlang_error>
      Error in `glubort()`: Can only supply one unnamed argument, not 2.
    Code
      (expect_error(df %>% ungroup() %>% do(1)))
    Output
      <error/rlang_error>
      Error in `glubort()`: Result must be a data frame, not numeric
    Code
      (expect_error(df %>% do(1)))
    Output
      <error/rlang_error>
      Error in `glubort()`: Results 1, 2, 3 must be data frames, not numeric
    Code
      (expect_error(df %>% do("a")))
    Output
      <error/rlang_error>
      Error in `glubort()`: Results 1, 2, 3 must be data frames, not character
    Code
      (expect_error(df %>% do(x = 1, 2)))
    Output
      <error/rlang_error>
      Error in `named_args()`: Arguments must either be all named or all unnamed.

