astyle <- function(extra_args = character()) {
  astyle_cmd <- "astyle"
  if (Sys.which(astyle_cmd) == "") {
    skip("astyle not found")
  }

  astyle_args <- c(
    "-n",
    "--indent=spaces=2",
    "--indent-namespaces",
    "--indent-preproc-block",
    "--unpad-paren",
    "--pad-header",
    "--min-conditional-indent=0",
    "--align-pointer=type",
    "--align-reference=type"
  )

  src_path <- normalizePath(vcapply(c("../../src", "../../inst/include"), testthat::test_path))
  src_files <- dir(src_path, "[.](?:cpp|h)$", recursive = TRUE, full.names = TRUE)
  astyle_files <- grep("(?:RcppExports[.]cpp|static_assert[.]h)", src_files, value = TRUE, invert = TRUE)
  output <- system2(astyle_cmd, c(astyle_args, astyle_files, extra_args), stdout = TRUE, stderr = TRUE)
  unchanged <- grepl("^Unchanged", output)
  if (any(!unchanged)) {
    warning(paste(output[!unchanged], collapse = "\n"))
  }
}
