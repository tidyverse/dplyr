combine_pair_test <- function(item_pair, var1, var2, result,
                              can_combine = TRUE, warning = FALSE) {
  label_if_fail <- paste0(
    "combine(items[c(\"", var1, "\", \"", var2, "\")])"
  )

  if (warning) {
    warning_regexp <- ".*"
  } else {
    warning_regexp <- NA
  }

  if (can_combine) {
    expect_warning(
      res <- combine(item_pair),
      regexp = warning_regexp,
      label = label_if_fail
    )
    expect_equal(
      object = res,
      expected = result,
      label = label_if_fail,
      expected.label = deparse(result)
    )
  } else {
    expect_warning(
      expect_error(
        combine(item_pair),
        "^Argument 2 can't be converted from [^ ]* to [^ ]*$",
        label = label_if_fail
      ),
      regexp = warning_regexp,
      label = label_if_fail
    )
  }
}

can_be_combined <- function(item1, item2,
                            class1, class2,
                            all_na1, all_na2,
                            known_to_dplyr1, known_to_dplyr2) {

  # Unknown classes will be stripped and ignored (#2406)
  if (!known_to_dplyr1) {
    class1 <- class(as.vector(item1))
  }
  if (!known_to_dplyr2) {
    class2 <- class(as.vector(item2))
  }

  # Two elements of the same class can be combined
  # NA values are also combinable
  if (identical(class1, class2) || all_na1 || all_na2) {
    return(TRUE)
  }

  # doubles and integers:
  if (all(c(class1, class2) %in% c("numeric", "integer"))) {
    return(TRUE)
  }

  # coerce factor with character
  if (
    (identical(class1, "factor") && identical(class2, "character")) ||
    (identical(class2, "factor") && identical(class1, "character"))
  ){
    return(TRUE)
  }

  # All the other cases can't be combined
  return(FALSE)
}

give_a_warning <- function(item1, item2,
                           class1, class2,
                           known_to_dplyr1, known_to_dplyr2,
                           can_be_combined) {
  # Unknown classes give a warning, because attributes may be wrong
  if (!known_to_dplyr1) {
    return(TRUE)
  }

  # If only the second element is of an unknown type to dplyr
  # Then the warning is only emmitted in case we can combine (otherwise the
  # error appears before)
  if (!known_to_dplyr2 && can_be_combined) {
    return(TRUE)
  }

  # factor and character give a warning when combined (coercion to character)
  if (
    (identical(class1, "factor") && identical(class2, "character")) ||
    (identical(class1, "character") && identical(class2, "factor"))
  ) {
    return(TRUE)
  }

  # Two factors give a warning if they don't have identical levels (coercion to character)
  if (identical(class1, "factor") && identical(class2, "factor")) {
    if (!identical(levels(item1), levels(item2))) {
      return(TRUE)
    }
  }
  # All other cases do not raise a warning
  return(FALSE)
}

combine_result <- function(item1, item2,
                           class1, class2,
                           all_na1, all_na2,
                           known_to_dplyr1, known_to_dplyr2,
                           can_combine, give_warning) {
  result <- NULL

  # Unknown classes will be stripped and ignored (#2406)
  if (!known_to_dplyr1) {
    class1 <- class(as.vector(item1))
  }
  if (!known_to_dplyr2) {
    class2 <- class(as.vector(item2))
  }

  if (can_combine) {
    # Custom coercions:
    # - Factor with character coerced to character
    # - Factor with Factor without same levels -> character
    # - Factor with NA is Factor
    # Otherwise use the default approach with unlist and add classes
    # if needed.
    if ((identical(class1, "factor") && identical(class2, "character")) ||
      (identical(class2, "factor") && identical(class1,"character"))) {
      result <- c(as.character(item1), as.character(item2))
    } else if ((identical(class1, "factor") && identical(class2, "factor")) &&
      !identical(levels(item1), levels(item2))) {
      result <- c(as.character(item1), as.character(item2))
    } else if ((is.factor(item1) && all(is.na(item2))) ||
      (is.factor(item2) && all(is.na(item1)))) {
      result <- factor(c(as.character(item1), as.character(item2)))
    } else {
      # Default combination result
      result <- unlist(
        list(item1, item2),
        recursive = FALSE,
        use.names = FALSE
      )

      # Add classes and attributes in some cases to the default
      if ((all(is.na(item1)) && "POSIXct" %in% class2) ||
        (all(is.na(item2)) && "POSIXct" %in% class1) ||
        ("POSIXct" %in% class1 && "POSIXct" %in% class2)) {
        class(result) <- c("POSIXct", "POSIXt")
        attr(result, "tzone") <- ""
      } else if (all_na1 && known_to_dplyr2) {
        class(result) <- class2
      } else if (all_na2 && known_to_dplyr1) {
        class(result) <- class1
      } else if (identical(class1, class2) && known_to_dplyr1) {
        class(result) <- class1
      }
    }
  }
  list(result)
}



prepare_table_with_coercion_rules <- function() {
  items <- list(
    logicalvalue = TRUE,
    logicalNA = NA,
    anotherNA = c(NA, NA),
    integer = 4L,
    factor = factor("a"),
    another_factor = factor("b"),
    double = 4.5,
    character = "c",
    POSIXct = as.POSIXct("2010-01-01"),
    Date = as.Date("2016-01-01"),
    complex = 1 + 2i,
    int_with_class = structure(4L, class = "int_with_class"),
    num_with_class = structure(4.5, class = "num_with_class")
  )

  special_non_vector_classes <- c(
    "factor", "POSIXct", "Date", "table", "AsIs", "integer64"
  )
  pairs <- expand.grid(names(items), names(items))
  pairs$can_combine <- FALSE
  pairs$warning <- FALSE
  pairs$item_pair <- vector("list", nrow(pairs))
  pairs$result <- vector("list", nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    item1 <- items[[pairs$Var1[i]]]
    item2 <- items[[pairs$Var2[i]]]
    class1 <- class(item1)
    class2 <- class(item2)
    all_na1 <- all(is.na(item1))
    all_na2 <- all(is.na(item2))
    known_to_dplyr1 <-
      is.vector(item1) ||
        any(class1 %in% special_non_vector_classes)
    known_to_dplyr2 <-
      is.vector(item2) ||
        any(class2 %in% special_non_vector_classes)

    pairs$can_combine[i] <- can_be_combined(
      item1, item2, class1, class2,
      all_na1, all_na2, known_to_dplyr1, known_to_dplyr2
    )

    pairs$warning[i] <- give_a_warning(
      item1, item2,
      class1, class2,
      known_to_dplyr1, known_to_dplyr2,
      can_be_combined = pairs$can_combine[i]
    )

    pairs$item_pair[[i]] <- list(item1, item2)
    pairs$result[i] <- combine_result(
      item1, item2, class1, class2,
      all_na1, all_na2,
      known_to_dplyr1, known_to_dplyr2,
      pairs$can_combine[i], pairs$warning[i]
    )
  }

  return(pairs)
}

print_pairs <- function(pairs) {
  pairs_printable <- pairs
  pairs_printable$result <- sapply(
    pairs$result,
    function(x) {
      if (is.null(x)) {
        ""
      } else {
        as.character(x)
      }
    }
  )
  pairs_printable$result_class <- lapply(
    pairs$result,
    function(x) {
      if (is.null(x)) {
        ""
      } else {
        class(x)
      }
    }
  )
  pairs_printable <- arrange(
    pairs_printable, desc(can_combine), warning, Var1, Var2
  )
  pairs_printable
}

combine_coercion_types <- function() {
  pairs <- prepare_table_with_coercion_rules()
  # knitr::kable(print_pairs(pairs))
  for (i in seq_len(nrow(pairs))) {
    test_that(paste0("Coercion from ", pairs$Var1[i], " to ", pairs$Var2[i]), {
      combine_pair_test(
        item_pair = pairs$item_pair[[i]],
        var1 = pairs$Var1[i],
        var2 = pairs$Var2[i],
        result = pairs$result[[i]],
        can_combine = pairs$can_combine[i],
        warning = pairs$warning[i]
      )
    })
  }
}
