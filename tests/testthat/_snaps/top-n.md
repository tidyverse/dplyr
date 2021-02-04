# top_n() quotes n

    Code
      res1 <- top_n(mtcars, n() * 0.5)
    Message <message>
      Selecting by carb

---

    Code
      res2 <- top_n(mtcars, 16)
    Message <message>
      Selecting by carb

# top_frac() is a shorthand for top_n(n()*)

    Code
      res1 <- top_n(mtcars, n() * 0.5)
    Message <message>
      Selecting by carb

---

    Code
      res2 <- top_frac(mtcars, 0.5)
    Message <message>
      Selecting by carb

