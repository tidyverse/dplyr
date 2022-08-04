# generate informative errors

    Code
      id()
    Condition
      Error:
      ! `id()` was deprecated in dplyr 0.5.0 and is now defunct.
      Please use `vctrs::vec_group_id()` instead.
    Code
      failwith()
    Condition
      Error:
      ! `failwith()` was deprecated in dplyr 0.7.0 and is now defunct.
      Please use `purrr::possibly()` instead.
    Code
      funs()
    Condition
      Error:
      ! `funs()` was deprecated in dplyr 0.8.0 and is now defunct.
      Please use a list of either functions or lambdas: 
      
        # Simple named list: 
        list(mean = mean, median = median)
      
        # Auto named with `tibble::lst()`: 
        tibble::lst(mean, median)
      
        # Using lambdas
        list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))

