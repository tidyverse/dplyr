#' Progress bar with estimated time.
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("deprecated")}
#'
#' This progress bar has been deprecated since providing progress bars is not
#' the responsibility of dplyr. Instead, you might try the more powerful
#' [progress](https://github.com/r-lib/progress) package.
#'
#' This reference class represents a text progress bar displayed estimated
#' time remaining. When finished, it displays the total duration.  The
#' automatic progress bar can be disabled by setting option
#' `dplyr.show_progress` to `FALSE`.
#'
#' @param n Total number of items
#' @param min_time Progress bar will wait until at least `min_time`
#'   seconds have elapsed before displaying any results.
#' @return A ref class with methods `tick()`, `print()`,
#'   `pause()`, and `stop()`.
#' @keywords internal
#' @export
#' @examples
#' p <- progress_estimated(3)
#' p$tick()
#' p$tick()
#' p$tick()
#'
#' p <- progress_estimated(3)
#' for (i in 1:3) p$pause(0.1)$tick()$print()
#'
#' p <- progress_estimated(3)
#' p$tick()$print()$
#'  pause(1)$stop()
#'
#' # If min_time is set, progress bar not shown until that many
#' # seconds have elapsed
#' p <- progress_estimated(3, min_time = 3)
#' for (i in 1:3) p$pause(0.1)$tick()$print()
#'
#' \dontrun{
#' p <- progress_estimated(10, min_time = 3)
#' for (i in 1:10) p$pause(0.5)$tick()$print()
#' }
progress_estimated <- function(n, min_time = 0) {
  lifecycle::deprecate_soft("1.0.0", "dplyr::progress_estimated()")

  Progress$new(n, min_time = min_time)
}

#' @importFrom R6 R6Class
Progress <- R6::R6Class("Progress",
  public = list(
    n = NULL,
    i = 0,
    init_time = NULL,
    stopped = FALSE,
    stop_time = NULL,
    min_time = NULL,
    last_update = NULL,

    initialize = function(n, min_time = 0, ...) {
      self$n <- n
      self$min_time <- min_time
      self$begin()
    },

    begin = function() {
      "Initialise timer. Call this before beginning timing."
      self$i <- 0
      self$last_update <- self$init_time <- now()
      self$stopped <- FALSE
      self
    },

    pause = function(x) {
      "Sleep for x seconds. Useful for testing."
      Sys.sleep(x)
      self
    },

    width = function() {
      getOption("width") - nchar("|100% ~ 99.9 h remaining") - 2
    },

    tick = function() {
      "Process one element"
      if (self$stopped) return(self)

      if (self$i == self$n) abort("No more ticks.")
      self$i <- self$i + 1
      self
    },

    stop = function() {
      if (self$stopped) return(self)

      self$stopped <- TRUE
      self$stop_time <- now()
      self
    },

    print = function(...) {
      if (!isTRUE(getOption("dplyr.show_progress")) || # user sepecifies no progress
        !interactive() || # not an interactive session
        !is.null(getOption("knitr.in.progress"))) { # dplyr used within knitr document
        return(invisible(self))
      }

      now_ <- now()
      if (now_ - self$init_time < self$min_time || now_ - self$last_update < 0.05) {
        return(invisible(self))
      }
      self$last_update <- now_

      if (self$stopped) {
        overall <- show_time(self$stop_time - self$init_time)
        if (self$i == self$n) {
          cat_line("Completed after ", overall)
          cat("\n")
        } else {
          cat_line("Killed after ", overall)
          cat("\n")
        }
        return(invisible(self))
      }

      avg <- (now() - self$init_time) / self$i
      time_left <- (self$n - self$i) * avg
      nbars <- trunc(self$i / self$n * self$width())

      cat_line(
        "|", str_rep("=", nbars), str_rep(" ", self$width() - nbars), "|",
        format(round(self$i / self$n * 100), width = 3), "% ",
        "~", show_time(time_left), " remaining"
      )

      invisible(self)
    }
  )
)

cat_line <- function(...) {
  msg <- paste(..., sep = "", collapse = "")
  gap <- max(c(0, getOption("width") - nchar(msg, "width")))
  cat("\r", msg, rep.int(" ", gap), sep = "")
  utils::flush.console()
}

str_rep <- function(x, i) {
  paste(rep.int(x, i), collapse = "")
}

show_time <- function(x) {
  if (x < 60) {
    paste(round(x), "s")
  } else if (x < 60 * 60) {
    paste(round(x / 60), "m")
  } else {
    paste(round(x / (60 * 60)), "h")
  }
}

now <- function() proc.time()[[3]]
