library(drake)
library(tidyverse)
library(glue)

options(repos = revdepcheck:::get_repos(FALSE))

get_this_pkg <- function() {
  desc::desc_get("Package") %>% unname()
}

get_base_pkgs <- function() {
  rownames(installed.packages(priority = "base"))
}

flatten <- . %>% unname() %>% unlist() %>% unique()

retry <- function(code, N = 1) {
  quo <- rlang::enquo(code)

  for (i in seq_len(N)) {
    ret <- tryCatch(rlang::eval_tidy(quo), error = identity)
    if (!inherits(ret, "error")) return(ret)
    Sys.sleep(runif(1) * 2)
  }

  stop(ret)
}

get_plan_deps <- function() {
  plan_deps <- drake_plan(
    available = available.packages(),
    this_pkg = get_this_pkg(),
    revdeps = tools::package_dependencies(this_pkg, available, 'most', reverse = TRUE) %>% flatten(),
    first_level_deps = tools::package_dependencies(revdeps, available, 'most'),
    all_level_deps = tools::package_dependencies(first_level_deps %>% flatten(), available, recursive = TRUE),
    base_pkgs = get_base_pkgs(),
    deps = c(revdeps, first_level_deps, all_level_deps) %>% flatten() %>% tools::package_dependencies(recursive = TRUE) %>% .[!(names(.) %in% base_pkgs)],
    strings_in_dots = "literals"
  )

  plan_deps
}
