#' @include query.r
NULL

# @examples
# billing <- "341409650721" # put your project number here
# samples <- src_bigquery("publicdata", "samples", billing)
# bq <- query(samples$con, ident("natality"))
# bq$submit()
# bq$wait()
# bq$fetch(1000)
# bq <- big_query(samples$con, "SELECT * FROM natality WHERE Year == 1979 LMIT 5")
# bq$submit()
# bq$wait()
# bq$fetch(100)
#' @export
query.bigquery <- function(con, sql, .vars) {
  assert_that(is.string(sql))

  BigQuery$new(con = con, sql = sql, .vars = .vars)
}
#
# BigQuery <- R6::R6Class("BigQuery", inherit = Query,
#   private = list(
#     job = NULL,
#     info = NULL
#   ),
#   public = list(
#     print = function(...) {
#       out <- paste0("<BigQuery> ",
#         if (is.null(private$job)) "unsubmitted" else "submitted", " ",
#         if (is.null(private$info)) "incomplete" else "complete", "\n",
#         sql, "\n"
#       )
#       cat(out)
#     },
#
#     # Submit query to job queue (idempotent; only submits once)
#     submit = function() {
#       if (!is.null(private$job) || !is.null(private$info)) return()
#
#       if (is.ident(self$sql)) {
#         # Special case if the sql is just a table name
#         private$info <- get_table(self$con$project, self$con$dataset, self$sql)
#       } else {
#         private$job <- insert_query_job(self$con$project, self$con$dataset,
#           self$sql, self$con$billing)
#       }
#
#       invisible(TRUE)
#     },
#
#     # Wait until job is complete
#     wait = function(quiet = FALSE, pause = 0.25) {
#       if (!is.null(private$info)) return(invisible())
#       private$job <- wait_for(private$job, quiet = quiet, pause = pause)
#
#       # Populate table info
#       dest <- job$configuration$query$destinationTable
#       private$info <- get_table(dest$projectId, dest$datasetId, dest$tableId)
#
#       invisible()
#     },
#
#     run = function(data = NULL, in_transaction = FALSE) {
#       self$submit()
#       self$wait()
#     },
#
#     fetch = function(n = -1L) {
#       self$run()
#
#       if (identical(n, -1L)) {
#         max_pages <- Inf
#         page_size <- 1e4
#       } else if (n < 1e4) {
#         max_pages <- 1
#         page_size <- n
#       } else {
#         max_pages <- ceiling(n / 1e4)
#         page_size <- 1e4
#       }
#
#       if (is.null(job)) {
#         list_tabledata(self$con$project, self$con$dataset, sql,
#           table_info = info, page_size = page_size, max_pages = max_pages)
#       } else {
#         dest <- private$job$configuration$query$destinationTable
#         list_tabledata(dest$projectId, dest$datasetId, dest$tableId,
#           table_info = info, page_size = page_size, max_pages = max_pages)
#       }
#     },
#
#     fetch_paged = function(chunk_size = 1e4, callback) {
#       run()
#
#       list_tabledata_callback(private$info$projectId, private$info$datasetId,
#         private$info$tableId, callback, table_info = info,
#         page_size = chunk_size, max_pages = Inf)
#     },
#
#     save_into = function(name = random_table_name()) {
#       run()
#       private$info$tableReference$tableId
#     },
#
#     vars = function() {
#       auto_names(self$.vars)
#     },
#
#     nrow = function() {
#       NA
#     },
#
#     ncol = function() {
#       length(self$vars)
#     }
#   )
# )

timer <- function() {
  start <- proc.time()[[3]]
  function() {
    proc.time()[[3]] - start
  }
}
