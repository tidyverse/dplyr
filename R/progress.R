#' Progress bar with estimated time.
#'
#' This reference class represents a text progress bar displayed estimated
#' time remaining. When finished, it displays the total duration.
#'
#' Progress bar will wait until at least \code{min_time} seconds have
#' elapsed before displaying any results.
#'
#' @keywords internal
#' @export Progress
#' @exportClass Progress
#' @examples
#' p <- Progress(3)
#' p$tick()
#' p$tick()
#' p$tick()
#'
#' p <- Progress(3)
#' for (i in 1:3) p$pause(0.1)$tick()$show()
#'
#' p <- Progress(3)
#' p$tick()$show()$
#'  pause(1)$stop()
#'
#' # If min_time is set, progress bar not shown until that many
#' # seconds have elapsed
#' p <- Progress(3, min_time = 3)
#' for (i in 1:3) p$pause(0.1)$tick()$show()
#'
#' \dontrun{
#' p <- Progress(10, min_time = 3)
#' for (i in 1:10) p$pause(0.5)$tick()$show()
#' }
Progress <- setRefClass("Progress",
  fields = list(
    n = "numeric",
    i = "numeric",
    init_time = "numeric",
    stopped = "logical",
    stop_time = "numeric",
    min_time = "numeric"
  ),
  methods = list(
    initialize = function(n, min_time = 0, ...) {
      initFields(n = n, min_time = min_time, stopped = FALSE, ...)
      begin()
    },

    begin = function() {
      "Initialise timer. Call this before beginning timing."
      i <<- 0
      init_time <<- now()
      stopped <<- FALSE
      .self
    },

    pause = function(x) {
      "Sleep for x seconds. Useful for testing."
      Sys.sleep(x)
      .self
    },

    width = function() {
      getOption("width") - nchar('|100% ~ 99.9 h remaining') - 2
    },

    tick = function() {
      "Process one element"
      if (stopped) return(.self)

      i <<- i + 1
      if (i == n) stop()
      .self
    },

    stop = function() {
      if (stopped) return(.self)

      stopped <<- TRUE
      stop_time <<- now()
      .self
    },

    show = function() {
      if (now() - init_time < min_time) {
        return(invisible(.self))
      }

      if (stopped) {
        overall <- show_time(stop_time - init_time)
        if (i == n) {
          cat_line("|", str_rep("=", width()), "|100%")
          cat("\nCompleted after", overall, "\n")
        } else {
          cat("\nKilled after", overall, "\n")
        }
        return(invisible(.self))
      }

      avg <- (now() - init_time) / i
      time_left <- (n - i) * avg
      nbars <- trunc(i / n * width())

      cat_line(
        "|", str_rep("=", nbars), str_rep(" ", width() - nbars), "|",
        format(round(i / n * 100), width = 3), "% ",
        "~", show_time(time_left), " remaining"
      )

      invisible(.self)
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
