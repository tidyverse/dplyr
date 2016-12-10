context("is_na")


test_that("is_na on databases as non-confusing IS NULL clause", {
  skip_if_no_sqlite()
  db <- src_sqlite(":memory:", TRUE)

  d <- data.frame(
    x = c(1, 2, 2),
    y = c(3, 5, NA),
    z = c(NA, 'a', 'b'),
    rowNum = 1:3,
    stringsAsFactors = FALSE
  )

  dfn1 <- copy_to(db, d , "dfn1")

  dres <- dfn1 %>% mutate(nna = 0) %>%
    mutate(nna = nna + is.na(x)) %>%
    mutate(nna = nna + is.na(y)) %>%
    mutate(nna = nna + is.na(z)) %>%
    arrange(rowNum) %>%
    collect()

  expect_equal(dres$nna, c(1, 0, 1))
})
