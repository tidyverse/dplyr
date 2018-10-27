.onLoad <- function(libname, pkgname) {
  op <- options()
  op.dplyr <- list(
    dplyr.show_progress = TRUE
  )
  toset <- !(names(op.dplyr) %in% names(op))
  if (any(toset)) options(op.dplyr[toset])

  local(envir = ns_env("dplyr"), {
    delayedAssign("env_bind_active", {
      if (utils::packageVersion("rlang") < "0.2.99") {
        env_get(ns_env("rlang"), "env_bind_fns")
      } else {
        env_get(ns_env("rlang"), "env_bind_active")
      }
    })
  })

  invisible()
}

.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("plyr", "attach"), function(...) {
    packageStartupMessage(rule())
    packageStartupMessage(
      "You have loaded plyr after dplyr - this is likely ",
      "to cause problems.\nIf you need functions from both plyr and dplyr, ",
      "please load plyr first, then dplyr:\nlibrary(plyr); library(dplyr)"
    )
    packageStartupMessage(rule())
  })
}

.onDetach <- function(libpath) {
  setHook(packageEvent("plyr", "attach"), NULL, "replace")
}

when_attached <- function(pkg, action) {
  if (is_attached(pkg)) {
    action
  } else {
    setHook(packageEvent(pkg, "attach"), function(...) action)
  }
}

is_attached <- function(pkg) paste0("package:", pkg) %in% search()
