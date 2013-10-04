#' A bigquery data source.
#'
#' @section Caching:
#' The variable names and number of rows are cached on source creation,
#' on the assumption that you're probably doing analysis on a table that's
#' not changing as you run queries. If this is not correct, then the values
#' of \code{dim} and \code{dimnames} may be out of date, but it shouldn't
#' otherwise affect operation.
#'
#' @examples
#' billing <- "341409650721" # put your project number here
#' samples <- src_bigquery("publicdata", "samples", billing)
#' births <- tbl(samples, "natality")
#' dim(births)
#' colnames(births)
#'
#' head(births)
#'
#' summarise(births, first_year = min(year), last_year = max(year))
#' date_info <- select(births, year:wday)
#' head(date_info)
#'
#' head(filter(select(births, year:wday), year > 2000))

#' by_year_sex <- group_by(births, year, is_male)
#' wt <- summarise(by_year_sex, n = count(), wt = mean(weight_pounds))
#' wtdf <- as.data.frame(wt)

src_bigquery <- function(project, dataset, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(billing))
  
  if (!require("bigrquery")) {
    stop("bigrquery package required to connect to bigquery db", call. = FALSE)
  }

  con <- structure(list(project = project, dataset = dataset,
    billing = billing), class = "bigquery")
  src_sql("sqlite", con)
}

#' @method tbl src_bigquery
#' @export
#' @rdname src_bigquery
tbl.src_sqlite <- function(src, from, ...) {
  tbl_sql("bigquery", src = src, from = from)
}

#' @S3method brief_desc src_sqlite
brief_desc.src_sqlite <- function(x) {
  paste0("bigquery [", x$con$project, "/", x$con$dataset, "]")
}

bigquery_sql <- sql_variant(
  "%||%" = sql_prefix("concat"),
  sd = sql_prefix("stddev"),
  
  # Casting
  as.logical = sql_prefix("boolean"),
  as.numeric = sql_prefix("float"),
  as.double = sql_prefix("float"),
  as.integer = sql_prefix("integer"),
  as.character = sql_prefix("string"),
  
  # Date/time
  Sys.date = sql_prefix("current_date"),
  Sys.time = sql_prefix("current_time"),
  
  # Regular expressions
  grepl = function(match, x) {
    sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
  },
  gsub = function(match, replace, x) {
    sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
      escape(replace))
  },
  
  # stringr equivalents
  str_detect = function(x, match) {
    sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
  },
  str_extract = function(x, match) {
    sprintf("REGEXP_EXTRACT(%s, %s)", escape(x), escape(match))
  },
  str_replace = function(x, match, replace) {
    sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
      escape(replace))
  }
)
