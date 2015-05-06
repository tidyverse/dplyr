#' Progress bar with estimated time.
#'
#' This reference class represents a text progress bar displayed estimated
#' time remaining. When finished, it displays the total duration.
#'
#' @param n Total number of
#' @param min_time Progress bar will wait until at least \code{min_time}
#'   seconds have elapsed before displaying any results.
#' @return A ref class with methods \code{tick()}, \code{print()},
#'   \code{pause()}, and \code{stop()}.
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

    initialize = function(n, min_time = 0, ...) {
      self$n <- n
      self$min_time <- min_time
      self$begin()
    },

    begin = function() {
      "Initialise timer. Call this before beginning timing."
      self$i <- 0
      self$init_time <- now()
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

      if (self$i == self$n) stop("No more ticks")
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
      if(!interactive() || !is.null(getOption('knitr.in.progress'))) {
        return(invisible(self))
      }
      if (now() - self$init_time < self$min_time) {
        return(invisible(self))
      }

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
  flush.console()
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
