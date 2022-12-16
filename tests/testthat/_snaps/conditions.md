# can pass verb-level error call

    Code
      mutate(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      transmute(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      summarise(group_by(mtcars, cyl), 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      i In group 1: `cyl = 4`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      filter(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      arrange(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `..1 = 1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      select(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      ! Problem while evaluating `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      slice(mtcars, 1 + "")
    Condition
      Error in `foo()`:
      i In argument: `1 + ""`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator

# can pass verb-level error call (example case)

    Code
      my_verb(mtcars, 1 + "", am)
    Condition
      Error in `my_verb()`:
      i In argument: `.result = (1 + "") * am`.
      Caused by error in `1 + ""`:
      ! non-numeric argument to binary operator
    Code
      my_verb(mtcars, cyl, c(am, vs))
    Condition
      Error in `my_verb()`:
      i In argument: `.result = cyl * c(am, vs)`.
      Caused by error:
      ! `.result` must be size 32 or 1, not 64.

# `err_locs()` works as expected

    Code
      err_locs(1.5)
    Condition
      Error in `err_locs()`:
      ! `x` must be an integer vector of locations.
      i This is an internal error that was detected in the dplyr package.
        Please report it at <https://github.com/tidyverse/dplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      err_locs(integer())
    Condition
      Error in `err_locs()`:
      ! `x` must have at least 1 location.
      i This is an internal error that was detected in the dplyr package.
        Please report it at <https://github.com/tidyverse/dplyr/issues> with a reprex (<https://https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      err_locs(1L)
    Output
      `c(1)`
    Code
      err_locs(1:5)
    Output
      `c(1, 2, 3, 4, 5)`
    Code
      err_locs(1:6)
    Output
      `c(1, 2, 3, 4, 5)` and 1 more
    Code
      err_locs(1:7)
    Output
      `c(1, 2, 3, 4, 5)` and 2 more

# errors during dots collection are not enriched (#6178)

    Code
      mutate(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      transmute(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      select(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      arrange(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"
    Code
      filter(mtcars, !!foobarbaz())
    Condition
      Error in `foobarbaz()`:
      ! could not find function "foobarbaz"

# warnings are collected for `last_dplyr_warnings()`

    Code
      # Ungrouped
      df %>% mutate(x = f()) %>% invisible()
    Condition
      Warning:
      There was 1 warning in `mutate()`.
      i In argument: `x = f()`.
      Caused by warning in `f()`:
      ! msg
    Code
      last_dplyr_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% mutate(x = f()) %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      

---

    Code
      # Grouped
      df %>% group_by(id) %>% mutate(x = f()) %>% invisible()
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In group 1: `id = 1`.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Code
      last_dplyr_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In group 1: `id = 1`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% group_by(id) %>% mutate(x = f()) %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In group 2: `id = 2`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% group_by(id) %>% mutate(x = f()) %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      

---

    Code
      # Rowwise
      df %>% rowwise() %>% mutate(x = f()) %>% invisible()
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Code
      last_dplyr_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% rowwise() %>% mutate(x = f()) %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 2.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% rowwise() %>% mutate(x = f()) %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      

---

    Code
      # Multiple type of warnings within multiple verbs
      df %>% group_by(g = f():n()) %>% rowwise() %>% mutate(x = f()) %>% group_by(id) %>%
        mutate(x = f()) %>% invisible()
    Condition
      Warning:
      There was 1 warning in `group_by()`.
      i In argument: `g = f():n()`.
      Caused by warning in `f()`:
      ! msg
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In group 1: `id = 1`.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Code
      last_dplyr_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `group_by()`:
      i In argument: `g = f():n()`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-... %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. +-dplyr::group_by(., id)
       4. +-dplyr::mutate(., x = f())
       5. +-dplyr::rowwise(.)
       6. +-dplyr::group_by(., g = f():n())
       7. \-dplyr:::group_by.data.frame(., g = f():n())
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-... %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. +-dplyr::group_by(., id)
       4. +-dplyr::mutate(., x = f())
       5. \-dplyr:::mutate.data.frame(., x = f())
      
      [[3]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 2.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-... %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. +-dplyr::group_by(., id)
       4. +-dplyr::mutate(., x = f())
       5. \-dplyr:::mutate.data.frame(., x = f())
      
      [[4]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In group 1: `id = 1`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-... %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      
      [[5]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In group 2: `id = 2`.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-... %>% invisible()
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      

---

    Code
      # Truncated (1 more)
      df %>% rowwise() %>% mutate(x = f())
    Condition
      Warning:
      There were 2 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
    Output
      # A tibble: 2 x 2
      # Rowwise: 
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
    Code
      last_dplyr_warnings(n = 1)
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% rowwise() %>% mutate(x = f())
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      
    Message
      ... with 1 more warning.
      i Run `dplyr::last_dplyr_warnings(n = 2)` to show more.

---

    Code
      # Truncated (several more)
      df <- tibble(id = 1:5)
      df %>% rowwise() %>% mutate(x = f())
    Condition
      Warning:
      There were 5 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      i Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
    Output
      # A tibble: 5 x 2
      # Rowwise: 
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
      3     3     1
      4     4     1
      5     5     1
    Code
      last_dplyr_warnings(n = 1)
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! msg
      ---
      Backtrace:
          x
       1. +-df %>% rowwise() %>% mutate(x = f())
       2. +-dplyr::mutate(., x = f())
       3. \-dplyr:::mutate.data.frame(., x = f())
      
    Message
      ... with 4 more warnings.
      i Run `dplyr::last_dplyr_warnings(n = 2)` to show more.

# complex backtraces with base and rlang warnings

    Code
      foo()
    Condition
      Warning:
      There was 1 warning in `group_by()`.
      i In argument: `x = f(1):n()`.
      Caused by warning in `h()`:
      ! foo
      Warning:
      There were 3 warnings in `mutate()`.
      The first warning was:
      i In argument: `x = f(1, base = FALSE)`.
      i In group 1: `x = 1`.
      Caused by warning:
      ! foo
      i Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
    Output
      # A tibble: 3 x 2
      # Groups:   x [1]
           id     x
        <int> <dbl>
      1     1     1
      2     2     1
      3     3     1
    Code
      last_dplyr_warnings()
    Output
      [[1]]
      <warning/rlang_warning>
      Warning in `group_by()`:
      i In argument: `x = f(1):n()`.
      Caused by warning in `h()`:
      ! foo
      ---
      Backtrace:
          x
       1. +-dplyr (local) foo()
       2. | \-dplyr (local) bar()
       3. |   \-df %>% group_by(x = f(1):n()) %>% mutate(x = f(1, base = FALSE))
       4. +-dplyr::mutate(., x = f(1, base = FALSE))
       5. +-dplyr::group_by(., x = f(1):n())
       6. \-dplyr:::group_by.data.frame(., x = f(1):n())
      
      [[2]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f(1, base = FALSE)`.
      i In group 1: `x = 1`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
          x
       1. +-dplyr (local) foo()
       2. | \-dplyr (local) bar()
       3. |   \-df %>% group_by(x = f(1):n()) %>% mutate(x = f(1, base = FALSE))
       4. +-dplyr::mutate(., x = f(1, base = FALSE))
       5. \-dplyr:::mutate.data.frame(., x = f(1, base = FALSE))
      
      [[3]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f(1, base = FALSE)`.
      i In group 2: `x = 2`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
          x
       1. +-dplyr (local) foo()
       2. | \-dplyr (local) bar()
       3. |   \-df %>% group_by(x = f(1):n()) %>% mutate(x = f(1, base = FALSE))
       4. +-dplyr::mutate(., x = f(1, base = FALSE))
       5. \-dplyr:::mutate.data.frame(., x = f(1, base = FALSE))
      
      [[4]]
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `x = f(1, base = FALSE)`.
      i In group 3: `x = 3`.
      Caused by warning:
      ! foo
      ---
      Backtrace:
          x
       1. +-dplyr (local) foo()
       2. | \-dplyr (local) bar()
       3. |   \-df %>% group_by(x = f(1):n()) %>% mutate(x = f(1, base = FALSE))
       4. +-dplyr::mutate(., x = f(1, base = FALSE))
       5. \-dplyr:::mutate.data.frame(., x = f(1, base = FALSE))
      

# can collect warnings in main verbs

    Code
      invisible(mtcars %>% rowwise() %>% filter(f()) %>% arrange(f()) %>% mutate(a = f()) %>%
        summarise(b = f()))
    Condition
      Warning:
      There were 32 warnings in `filter()`.
      The first warning was:
      i In argument: `f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! foo
      i Run `dplyr::last_dplyr_warnings()` to see the 31 remaining warnings.
      Warning:
      There was 1 warning in `arrange()`.
      i In argument: `..1 = f()`.
      Caused by warning in `f()`:
      ! foo
      Warning:
      There were 32 warnings in `mutate()`.
      The first warning was:
      i In argument: `a = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! foo
      i Run `dplyr::last_dplyr_warnings()` to see the 31 remaining warnings.
      Warning:
      There were 32 warnings in `summarise()`.
      The first warning was:
      i In argument: `b = f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! foo
      i Run `dplyr::last_dplyr_warnings()` to see the 31 remaining warnings.
    Code
      warnings <- last_dplyr_warnings(Inf)
      warnings[[1]]
    Output
      <warning/rlang_warning>
      Warning in `filter()`:
      i In argument: `f()`.
      i In row 1.
      Caused by warning in `f()`:
      ! foo
    Code
      warnings[[33]]
    Output
      <warning/rlang_warning>
      Warning in `arrange()`:
      i In argument: `..1 = f()`.
      Caused by warning in `f()`:
      ! foo
    Code
      warnings[[65]]
    Output
      <warning/rlang_warning>
      Warning in `mutate()`:
      i In argument: `a = f()`.
      i In row 32.
      Caused by warning in `f()`:
      ! foo
    Code
      warnings[[97]]
    Output
      <warning/rlang_warning>
      Warning in `summarise()`:
      i In argument: `b = f()`.
      i In row 32.
      Caused by warning in `f()`:
      ! foo

