# funs() is deprecated

    Code
      funs(fn = bar)
    Warning <lifecycle_warning_deprecated>
      `funs()` is deprecated as of dplyr 0.8.0.
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

    `function(si) {
        mp[si]
    }` must be a function name (quoted or unquoted) or an unquoted call, not `function`.

---

    `~mp[.]` must be a function name (quoted or unquoted) or an unquoted call, not `~`.

