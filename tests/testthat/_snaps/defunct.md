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
    Code
      select_vars()
    Condition
      Error:
      ! `select_vars()` was deprecated in dplyr 0.8.4 and is now defunct.
      Please use `tidyselect::vars_select()` instead.
    Code
      rename_vars()
    Condition
      Error:
      ! `rename_vars()` was deprecated in dplyr 0.8.4 and is now defunct.
      Please use `tidyselect::vars_rename()` instead.
    Code
      select_var()
    Condition
      Error:
      ! `select_var()` was deprecated in dplyr 0.8.4 and is now defunct.
      Please use `tidyselect::vars_pull()` instead.
    Code
      current_vars()
    Condition
      Error:
      ! `current_vars()` was deprecated in dplyr 0.8.4 and is now defunct.
      Please use `tidyselect::peek_vars()` instead.

