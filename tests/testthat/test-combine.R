context("combine")

test_that("combine handles NULL (1596)", {
    expect_equal( combine(list(NULL, 1,2)), c(1,2) )
    expect_equal( combine(list(1,NULL,2)), c(1,2) )
    expect_equal( combine(list(1,2,NULL)), c(1,2) )
    expect_error( combine(list(NULL,NULL)))
})

test_that("combine complains about incompatibilites", {
  expect_error(combine("a", 1), "character to numeric")
  expect_error(combine(factor("a"), 1L), "factor to integer")
})

test_that("combine works with input that used to fail (#1780)", {
  no <- list(alpha = letters[1:3], omega = letters[24:26])
  expect_equal(combine(no), unlist(no, use.names = FALSE))
})

test_that("combine works with NA and logical (#2203)", {
  # NA first
  expected_result <- c(NA, TRUE, FALSE, NA, TRUE)
  works1 <- combine(list(NA, TRUE, FALSE, NA, TRUE))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- c(TRUE, FALSE, NA, TRUE)
  works1 <- combine(list(TRUE, FALSE, NA, TRUE))
  expect_equal(works1, expected_result)

  # NA length > 1
  expected_result <- c(TRUE, FALSE, NA, NA, TRUE)
  works3 <- combine(list(TRUE, FALSE, c(NA, NA), TRUE))
  expect_equal(works3, expected_result)

})

test_that("combine works with NA and integers (#2203)", {
  works <- combine(list(1L, 2L, NA, 4L))
  expect_equal(works, c(1L, 2L, NA, 4L))
  works <- combine(list(1L, 2L, c(NA, NA), 4L))
  expect_equal(works, c(1L, 2L, NA, NA, 4L))
})

test_that("combine works with NA and factors (#2203)", {
  # NA first
  fac <- factor(c("a", "c", NA, "b"), levels = letters[1:3])
  expected_result <- fac[c(3, 1, 3, 2)]
  works1 <- combine(list(NA, fac[1], NA, fac[2]))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- fac
  works1 <- combine(list(fac[1], fac[2], fac[3], fac[4]))
  expect_equal(works1, expected_result)

  works2 <- combine(list(fac[1], fac[2], NA, fac[4]))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- fac[c(1, 2, 3, 3, 4)]

  works3 <- combine(list(fac[1], fac[2], fac[c(3,3)], fac[4]))
  expect_equal(works3, expected_result)

  works4 <- combine(list(fac[1], fac[2], c(NA, NA), fac[4]))
  expect_equal(works4, expected_result)
})

test_that("combine works with NA and double (#2203)", {
  # NA first
  works <- combine(list(NA, 1.5, 2.5, NA, 4.5))
  expect_equal(works, c(NA, 1.5, 2.5, NA, 4.5))
  # NA length 1
  works <- combine(list(1.5, 2.5, NA, 4.5))
  expect_equal(works, c(1.5, 2.5, NA, 4.5))
  # NA length > 1
  works <- combine(list(1.5, 2.5, c(NA, NA), 4.5))
  expect_equal(works, c(1.5, 2.5, NA, NA, 4.5))
})

test_that("combine works with NA and characters (#2203)", {
  # NA first
  works <- combine(list(NA, "a", "b", "c", NA, "e"))
  expect_equal(works, c(NA, "a", "b", "c", NA, "e"))
  # NA length 1
  works <- combine(list("a", "b", "c", NA, "e"))
  expect_equal(works, c("a", "b", "c", NA, "e"))
  # NA length > 1
  works <- combine(list("a", "b", "c", c(NA, NA), "e"))
  expect_equal(works, c("a", "b", "c", NA, NA, "e"))
})


test_that("combine works with NA and POSIXct (#2203)", {
  # NA first
  works <- combine(list(NA, as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
                        NA, as.POSIXct("2010-01-04")))
  expect_equal(works, c(as.POSIXct(c(NA, "2010-01-01", "2010-01-02",
                                     NA, "2010-01-04"))))
  # NA length 1
  works <- combine(list(as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
                        NA, as.POSIXct("2010-01-04")))
  expect_equal(works, c(as.POSIXct(c("2010-01-01", "2010-01-02",
                                     NA, "2010-01-04"))))
  # NA length > 1
  works <- combine(list(as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
                        c(NA, NA), as.POSIXct("2010-01-04")))
  expect_equal(works, c(as.POSIXct(c("2010-01-01", "2010-01-02",
                                     NA, NA, "2010-01-04"))))
})

test_that("combine works with NA and Date (#2203)", {
  # NA first
  expected_result <- as.Date("2010-01-01") + c(NA, 1, 2, NA, 4)
  expect_equal(combine(as.list(expected_result)), expected_result)

  # NA length == 1
  expected_result <- c(as.Date(c("2010-01-01", "2010-01-02", NA, "2010-01-04")))
  works1 <- combine(list(as.Date("2010-01-01"), as.Date("2010-01-02"),
                         as.Date(NA), as.Date("2010-01-04")))
  expect_equal(works1, expected_result)

  works2 <- combine(list(as.Date("2010-01-01"), as.Date("2010-01-02"),
                         NA, as.Date("2010-01-04")))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- as.Date("2010-01-01") + c(0, 1, NA, NA, 3)
  works1 <- combine(split(expected_result, c(1,2,3,3,4)))
  expect_equal(works1, expected_result)

  works2 <- combine(list(as.Date("2010-01-01"), as.Date("2010-01-02"),
                         c(NA, NA),
                         as.Date("2010-01-04")))
  expect_equal(works2, expected_result)
})


test_that("combine works with NA and complex (#2203)", {
  # NA first
  expected_result <- c(NA, 1 + 2i)
  works1 <- combine(list(NA, 1 + 2i))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- c(1, 2, NA, 4) + 1i

  expect_equal(combine(as.list(expected_result)), expected_result)

  works2 <- combine(list(1 + 1i, 2 + 1i, NA, 4 + 1i))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- c(1, 2, NA, NA, 4) + 1i
  expect_equal(combine(split(expected_result, c(1, 2, 3, 3, 4))),
               expected_result)

  works3 <- combine(list(1 + 1i, 2 + 1i, c(NA, NA), 4 + 1i))
  expect_equal(works3, expected_result)
})

combine_pair_test <- function(item_pair, var1, var2, result,
                              can_combine = TRUE, warning = FALSE)  {
  if (can_combine) {
    if (warning) {
      expect_warning(res <- combine(item_pair),
                     label = paste0("combine(items[c(\"", var1, "\", \"", var2, "\")])"))
      expect_equal(object = res,
                   expected = result,
                   label = paste0("combine(items[c(\"", var1, "\", \"", var2, "\")])"),
                   expected.label = deparse(result))
    } else {
      expect_equal(object = combine(item_pair),
                   expected = result,
                   label = paste0("combine(items[c(\"", var1, "\", \"", var2, "\")])"),
                   expected.label = deparse(result))
    }
  } else {
    expect_error(suppressWarnings(combine(item_pair)),
                 label = paste0("combine(items[c(\"", var1, "\", \"", var2, "\")])"))
  }
}


prepare_table_with_coercion_rules <- function(items) {
  pairs <- expand.grid(names(items), names(items))
  pairs$can_combine <- FALSE
  pairs$warning <- FALSE
  pairs$result <- vector("list", nrow(pairs))

  ######## Can combine with items of the same type and class:
  pairs$can_combine[pairs$Var1 == pairs$Var2] <- TRUE

  # However, combining custom integer or numeric classes gives a warning, as
  # attributes may be lost in the way:
  pairs$warning[pairs$Var1 == "int_with_class" & pairs$Var2 == "int_with_class"] <- TRUE
  pairs$warning[pairs$Var1 == "num_with_class" & pairs$Var2 == "num_with_class"] <- TRUE

  ####### NA combines with anything, and anything combines with NA
  pairs$can_combine[pairs$Var1 == "logicalNA" | pairs$Var2 == "logicalNA"] <- TRUE
  # Except custom classes: We do not know what attributes should be attached if
  # the result has a NA, so we must warn.
  pairs$warning[pairs$Var1 == "int_with_class" & pairs$Var2 == "logicalNA"] <- TRUE
  pairs$warning[pairs$Var1 == "logicalNA" & pairs$Var2 == "int_with_class"] <- TRUE
  pairs$warning[pairs$Var1 == "logicalNA" & pairs$Var2 == "num_with_class"] <- TRUE
  pairs$warning[pairs$Var1 == "num_with_class" & pairs$Var2 == "logicalNA"] <- TRUE

  ####### Coerce doubles and integers:
  pairs$can_combine[pairs$Var1 == "integer" & pairs$Var2 == "double"] <- TRUE
  pairs$can_combine[pairs$Var1 == "double" & pairs$Var2 == "integer"] <- TRUE

  ####### factor and character  (factor-character promotes to character)
  pairs$can_combine[pairs$Var1 == "factor" & pairs$Var2 == "character"] <- TRUE
  pairs$can_combine[pairs$Var1 == "character" & pairs$Var2 == "factor"] <- TRUE

  # The result usually is equal to unlist
  pairs$result <- lapply(seq_len(nrow(pairs)), function(i) {
    if (pairs$can_combine[i]) {
      unlist(items[c(pairs$Var1[i], pairs$Var2[i])], recursive = FALSE, use.names = FALSE)
    } else {
      NULL
    }
  })

  # But unlist does not get it right all the times:

  # factor and character are converted to character
  pairs$result[pairs$Var1 == "factor" & pairs$Var2 == "character"] <- list(c("a", "b"))
  pairs$warning[pairs$Var1 == "factor" & pairs$Var2 == "character"] <- TRUE
  pairs$result[pairs$Var1 == "character" & pairs$Var2 == "factor"] <- list(c("b", "a"))

  # logical-POSIXct: unlist does not preserve class and tzone attributes
  attr(pairs$result[pairs$Var1 == "logicalNA" & pairs$Var2 == "POSIXct"][[1]],
       c("class", "tzone")) <- c("POSIXct", "")
  attr(pairs$result[pairs$Var1 == "POSIXct" & pairs$Var2 == "logicalNA"][[1]],
       c("class", "tzone")) <- c("POSIXct", "")
  # factor-NA: (unlist would drop the class)
  pairs$result[pairs$Var1 == "factor" & pairs$Var2 == "logicalNA"] <- list(factor(c("a", NA)))
  pairs$result[pairs$Var1 == "logicalNA" & pairs$Var2 == "factor"] <- list(factor(c(NA, "a")))
  # POSIXct (unlist would drop the class and tzone attributes)
  class(pairs$result[pairs$Var1 == "POSIXct" & pairs$Var2 == "POSIXct"][[1]]) <- c("POSIXct", "POSIXt")
  attr(pairs$result[pairs$Var1 == "POSIXct" & pairs$Var2 == "POSIXct"][[1]], "tzone") <- ""
  class(pairs$result[pairs$Var1 == "POSIXct" & pairs$Var2 == "logicalNA"][[1]]) <- c("POSIXct", "POSIXt")
  attr(pairs$result[pairs$Var1 == "POSIXct" & pairs$Var2 == "logicalNA"][[1]], "tzone") <- ""
  class(pairs$result[pairs$Var1 == "logicalNA" & pairs$Var2 == "POSIXct"][[1]]) <- c("POSIXct", "POSIXt")
  attr(pairs$result[pairs$Var1 == "logicalNA" & pairs$Var2 == "POSIXct"][[1]], "tzone") <- ""
  # unlist drops the class attribute in Date int_with_class and num_with_class:
  for (add_class_to in c("Date", "int_with_class", "num_with_class")) {
    class(pairs$result[pairs$Var1 == add_class_to & pairs$Var2 == add_class_to][[1]]) <- add_class_to
    class(pairs$result[pairs$Var1 == add_class_to & pairs$Var2 == "logicalNA"][[1]]) <- add_class_to
    class(pairs$result[pairs$Var1 == "logicalNA" & pairs$Var2 == add_class_to][[1]]) <- add_class_to
  }
  return(pairs)
}

print_pairs <- function(pairs) {
  pairs_printable <- pairs
  pairs_printable$result <- sapply(pairs$result,
                                   function(x) if (is.null(x)) {""} else {as.character(x)})
  pairs_printable$result_class <- lapply(seq_len(nrow(pairs)), function(i) {
    if (is.null(pairs$result[[i]])) {
      ""
    } else {
      class(pairs$result[[i]])
    }
  })
  pairs_printable <- arrange(pairs_printable, desc(can_combine), warning, Var1, Var2)
  pairs_printable
}

combine_coercion_types <- function() {
  items <- list(logicalvalue = TRUE, logicalNA = NA, integer = 4L,
                factor = factor("a"), double = 4.5,
                character = "b", POSIXct = as.POSIXct("2010-01-01"),
                Date = as.Date("2016-01-01"), complex = 1 + 2i,
                int_with_class = structure(4L, class = "int_with_class"),
                num_with_class = structure(4.5, class = "num_with_class"))

  pairs <- prepare_table_with_coercion_rules(items)
  # knitr::kable(print_pairs(pairs))
  for (i in seq_len(nrow(pairs))) {
    test_that(paste("Coercion from", pairs$Var1[i], " to ", pairs$Var2[i]), {
      combine_pair_test(item_pair = items[c(pairs$Var1[i], pairs$Var2[i])],
                        var1 = pairs$Var1[i], var2 = pairs$Var2[i],
                        result = pairs$result[[i]],
                        can_combine = pairs$can_combine[i],
                        warning = pairs$warning[i])
    })
  }
}

combine_coercion_types()
