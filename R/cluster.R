cluster_env <- new.env(parent = emptyenv())

#' Cluster management.
#'
#' For parallel operations in \code{\link{do}}, dplyr maintains a light-weight
#' local cluster. Cluster creation is relatively expensive (around a second)
#' so the cluster is created once and cached for subsequent use.
#'
#' On windows, this is a PSOCK cluster, and on linux/mac it's a fork cluster.
#' If not supplied, the number of cores will be two less than the number of
#' cores provided by \code{\link[parallel]{detectCores}}}.
#'
#' \code{get_cluster}, \code{set_cluster} and \code{has_cluster} are low
#' level accessor functions to control the cluster cache. You shouldn't
#' need to use these unless you want to create your own cluster using another
#' mechanism. \code{init_cluster} creates and caches a new cluster, and
#' \code{stop_cluster} shuts it down.
#'
#' @keywords internal
#' @name dplyr-cluster
NULL

#' @export
#' @rdname dplyr-cluster
set_cluster <- function(x) {
  if (!inherits(x, "cluster")) {
    stop("x is not a cluster", call. = FALSE)
  }

  stop_cluster()
  cluster_env$cluster <- x

  invisible()
}

#' @export
#' @rdname dplyr-cluster
#' @importFrom parallel stopCluster
stop_cluster <- function() {
  if (has_cluster()) {
    stopCluster(cluster_env$cluster)
    cluster_env$cluster <- NULL
  }

  invisible()
}

#' @export
#' @rdname dplyr-cluster
get_cluster <- function() {
  cluster_env$cluster
}

#' @export
#' @rdname dplyr-cluster
has_cluster <- function() {
  exists("cluster", where = cluster_env)
}

#' @importFrom parallel detectCores makePSOCKcluster makeForkCluster
#' @export
#' @rdname dplyr-cluster
init_cluster <- function(cores = NA, quiet = FALSE) {
  if (has_cluster()) {
    return(invisible(get_cluster()))
  }

  if (is.na(cores)) {
    max <- parallel::detectCores()
    if (max == 1L) {
      stop("Single core computer. Parallelisation not available", call. = FALSE)
    }
    # Always use at least two cores, but leave two free for other work
    cores <- pmax(2L, max - 2L)
  }

  if (!quiet) message("Initialising ", cores, " core cluster.")
  windows <- .Platform$OS.type == "windows"
  if (windows) {
    cluster <- parallel::makePSOCKcluster(cores)
  } else {
    cluster <- parallel::makeForkCluster(cores)
  }

  set_cluster(cluster)
  invisible(cluster)
}

#' Cluster lapply.
#'
#' A low-level implementation of \code{\link{lapply}} which splits tasks
#' tasks across nodes in a cluster.
#'
#' @param .x list of elements to apply over
#' @param .f function to apply
#' @param ... additional arguments to \code{.f}
#' @param .cl cluster to use
#' @keywords internal
#' @examples
#' lapply_cluster(1:10, sqrt)
lapply_cluster <- function (.x, .f, ..., .cl = init_cluster()) {
  n <- length(.x)
  p <- min(length(.cl), n)

  if (n == 0) return()

  i <- 1
  j <- 1
  submit <- function() {
    parallel:::sendCall(.cl[[j]], .f, list(.x[[i]], ...), tag = i)
    i <<- i + 1
    j <<- j %% p + 1
  }
  recieve <- function() {
    parallel:::recvOneResult(.cl)
  }

  # Submit first round of jobs
  for (i in seq_len(p)) submit()

  # Wait for response, submitting new jobs as needed
  out <- vector("list", n)
  for (k in seq_len(n)) {
    d <- recieve()
    out[d$tag] <- list(d$value)

    if (i <= n) submit()
  }
  out
}
