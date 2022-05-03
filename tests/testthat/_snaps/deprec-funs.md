# funs() is deprecated

    Code
      funs(fn = bar)
    Condition
      Warning:
      `funs()` was deprecated in dplyr 0.8.0.
      Please use a list of either functions or lambdas: 
      
        # Simple named list: 
        list(mean = mean, median = median)
      
        # Auto named with `tibble::lst()`: 
        tibble::lst(mean, median)
      
        # Using lambdas
        list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    Output
      <fun_calls>
      $ fn: bar(.)

# funs() give meaningful error messages

    Code
      (expect_error(funs(function(si) {
        mp[si]
      })))
    Output
      <error/rlang_error>
      Error in `funs()`:
      ! `function(si) { mp[si] }` must be a function name (quoted or unquoted) or an unquoted call, not `function`.
    Code
      (expect_error(funs(~ mp[.])))
    Output
      <error/rlang_error>
      Error in `funs()`:
      ! `~mp[.]` must be a function name (quoted or unquoted) or an unquoted call, not `~`.

