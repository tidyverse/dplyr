# bind_cols repairs names

    Code
      bound <- bind_cols(df, df)
    Message <simpleMessage>
      New names:
      * a -> a...1
      * b -> b...2
      * a -> a...3
      * b -> b...4

# bind_cols() handles unnamed list with name repair (#3402)

    Code
      df <- bind_cols(list(1, 2))
    Message <simpleMessage>
      New names:
      * NA -> ...1
      * NA -> ...2

# *_bind() give meaningful errors

    Code
      bind_rows(df1, df2, .id = 5)
    Error <rlang_error>
      `.id` must be a scalar string, not a double vector of length 1.

---

    Code
      bind_rows(ll)
    Error <rlang_error>
      Argument 1 must have names.

---

    Code
      bind_rows(ll)
    Error <rlang_error>
      Argument 2 must be a data frame or a named atomic vector.

---

    Code
      bind_rows(df1, df2)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$a` <factor<4d52a>> and `..2$a` <integer>.

---

    Code
      bind_rows(df1, df3)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$a` <factor<4d52a>> and `..2$a` <double>.

---

    Code
      bind_rows(df1, df3)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$b` <double> and `..2$b` <factor<a022a>>.

---

    Code
      bind_rows(df1, df4)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$b` <double> and `..2$b` <character>.

---

    Code
      bind_rows(df2, df3)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$b` <integer> and `..2$b` <factor<a022a>>.

---

    Code
      bind_rows(df2, df4)
    Error <vctrs_error_incompatible_type>
      Can't combine `..1$b` <integer> and `..2$b` <character>.

---

    Code
      bind_rows(1:2)
    Error <rlang_error>
      Argument 1 must have names.

---

    Code
      bind_cols(a = 1:2, mtcars)
    Error <vctrs_error_incompatible_size>
      Can't recycle `a` (size 2) to match `..2` (size 32).

---

    Code
      bind_cols(mtcars, a = 1:3)
    Error <vctrs_error_incompatible_size>
      Can't recycle `..1` (size 32) to match `a` (size 3).

