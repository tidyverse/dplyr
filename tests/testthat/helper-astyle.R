astyle <- function(extra_args = character()) {
  astyle_cmd <- "astyle"
  if (Sys.which(astyle_cmd) == "") {
    skip("astyle not found")
  }

  astyle_args <- c(
    "-n",
    "--indent=spaces=2",
    "--indent-namespaces",
    "--unpad-paren",
    "--pad-header",
    "--min-conditional-indent=0",
    "--align-pointer=type",
    "--align-reference=type"
  )

  src_path <- normalizePath(testthat::test_path("../../src"))
  astyle_files <- file.path(src_path, setdiff(dir(src_path, "[.]cpp$"), "RcppExports.cpp"))
  output <- system2(astyle_cmd, c(astyle_args, astyle_files, extra_args), stdout = TRUE, stderr = TRUE)
  unchanged <- grepl("^Unchanged", output)
  if (any(!unchanged)) {
    warning(paste(output[!unchanged], collapse = "\n"))
  }
}
