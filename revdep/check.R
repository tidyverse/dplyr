if (Sys.getenv("DISPLAY") == "") stop("Run with xvfb-run")

free <- system(paste0("df --output=avail ", tempdir(), " | tail -n 1"), intern = TRUE)
if (as.numeric(free) < 1e8) stop("Set TMPDIR to a location with at least 100 GB free space")

package <- basename(getwd())

library(revdepcheck)

dir_setup(getwd())
if (!revdepcheck:::db_exists(getwd())) {
  revdepcheck:::db_setup(getwd())
}


if (length(revdep_todo()) == 0) {
  import_revdeps <- revdepcheck:::cran_revdeps(package = package, dependencies = c("Depends", "Imports"), bioc = TRUE)
  import_revdeps <- setdiff(import_revdeps, package)
  todo_import_revdeps <- import_revdeps

  while (FALSE && length(todo_import_revdeps) > 0) {
    print(length(todo_import_revdeps))
    print(todo_import_revdeps)
    print(Sys.time())
    new_import_revdeps <- unlist(purrr::map(todo_import_revdeps, revdepcheck:::cran_revdeps, dependencies = c("Depends", "Imports"), bioc = TRUE))
    todo_import_revdeps <- setdiff(new_import_revdeps, import_revdeps)
    import_revdeps <- union(import_revdeps, new_import_revdeps)
    print(new_import_revdeps)

    break # only one level for now
  }

  weak_revdeps <- revdepcheck:::cran_revdeps(package = package, dependencies = c("Suggests", "Enhances", "LinkingTo"), bioc = TRUE)
  print(weak_revdeps)

  revdep_add(".", c(import_revdeps, weak_revdeps))
}

options(repos = revdepcheck:::get_repos(bioc = TRUE))

todo <- revdep_todo()
withr::with_temp_libpaths(action = "replace", {
  crancache::install_packages(c(
    todo,
    "pillar"
  ))

  remotes::install_local(".")
})

repos <- paste0(
  "file://",
  file.path(crancache::get_cache_dir(), c("cran-bin", "bioc-bin", "other-bin"))
)

names(repos) <- c("CRAN", "Bioc", "other")

options(repos = repos)

N <- 100
for (i in seq_len(N)) {
  try(
    revdepcheck::revdep_check(
      bioc = TRUE,
      dependencies = character(),
      quiet = FALSE,
      num_workers = 24,
      timeout = as.difftime(60, units = "mins")
    )
  )

  if (length(revdep_todo()) == 0) break
}

options(repos = revdepcheck:::get_repos(bioc = TRUE))

withr::with_output_sink(
  "revdep/cran.md",
  revdep_report_cran()
)

system("git add revdep/*.md")
system("git commit -m 'update revdep results'")
system("git push -u origin HEAD")
