vcapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X = X, FUN = FUN, FUN.VALUE = character(1L), ..., USE.NAMES = USE.NAMES)
}

astyle <- function(extra_args = character()) {
  astyle_cmd <- "astyle"
  if (Sys.which(astyle_cmd) == "") {
    skip("astyle not found")
  }

  astyle_args <- c(
    "-n",
    "--indent=spaces=2",
    "--unpad-paren",
    "--pad-header",
    "--pad-oper",
    "--min-conditional-indent=0",
    "--align-pointer=type",
    "--align-reference=type"
  )

  src_path <- normalizePath(map_chr(c("../../src"), testthat::test_path))
  src_files <- dir(src_path, "[.](?:cpp|h)$", recursive = TRUE, full.names = TRUE)
  astyle_files <- grep("(?:RcppExports[.](?:cpp|h)|static_assert[.]h)", src_files, value = TRUE, invert = TRUE)
  output <- system2(astyle_cmd, c(astyle_args, astyle_files, extra_args), stdout = TRUE, stderr = TRUE)
  unchanged <- grepl("^Unchanged", output)
  if (any(!unchanged)) {
    rlang::warn(paste(output[!unchanged], collapse = "\n"))
  }
}
