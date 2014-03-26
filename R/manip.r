#' Data manipulation functions.
#'
#' These five functions form the backbone of dplyr. They are all S3 generic
#' functions with methods for each individual data type. All functions work
#' exactly the same way: the first argument is the tbl, and the
#' subsequence arguments are interpreted in the context of that tbl.
#'
#' @section Manipulation functions:
#'
#' The five key data manipulation functions are:
#'
#' \itemize{
#'   \item filter: return only a subset of the rows. If multiple conditions are
#'     supplied they are combined with \code{&}.
#'   \item select: return only a subset of the columns. If multiple columns are
#'     supplied they are all used.
#'   \item arrange: reorder the rows. Multiple inputs are ordered from left-to-
#'    right.
#'   \item mutate: add new columns. Multiple inputs create multiple columns.
#'   \item summarise: reduce each group to a single row. Multiple inputs create
#'     multiple output summaries.
#' }
#'
#' These are all made significantly more useful when applied by group,
#' as with \code{\link{group_by}}
#'
#' @section Tbls:
#'
#' dplyr comes with three built-in tbls.  Read the help for the
#' manip methods of that class to get more details:
#'
#' \itemize{
#'   \item data.frame: \link{manip_df}
#'   \item data.table: \link{manip_dt}
#'   \item SQLite: \code{\link{src_sqlite}}
#'   \item PostgreSQL: \code{\link{src_postgres}}
#'   \item MySQL: \code{\link{src_mysql}}
#' }
#'
#' @section Output:
#'
#' Generally, manipulation functions will return an output object of the
#' same type as their input. The exceptions are:
#'
#' \itemize{
#'    \item \code{summarise} will return an ungrouped source
#'    \item remote sources (like databases) will typically return a local
#'      source from at least \code{summarise} and \code{mutate}
#' }
#' @name manip
#' @param .data a tbl
#' @param ... variables interpreted in the context of that data frame.
#' @examples
#' filter(mtcars, cyl == 8)
#' select(mtcars, mpg, cyl, hp:vs)
#' arrange(mtcars, cyl, disp)
#' mutate(mtcars, displ_l = disp / 61.0237)
#' summarise(mtcars, mean(disp))
#' summarise(group_by(mtcars, cyl), mean(disp))
NULL

#' @rdname manip
#' @export
filter <- function(.data, ...) UseMethod("filter")

#' @rdname manip
#' @export
summarise <- function(.data, ...) UseMethod("summarise")
#' @rdname manip
#' @export
summarize <- summarise

#' @rdname manip
#' @export
mutate <- function(.data, ...) UseMethod("mutate")

#' @rdname manip
#' @export
arrange <- function(.data, ...) UseMethod("arrange")

#' @section Selection:
#' As well as using existing functions like \code{:} and \code{c}, there are
#' a number of special functions that only work inside \code{select}
#'
#' \itemize{
#'  \item \code{starts_with(x, ignore.case = FALSE)}:
#'    names starts with \code{x}
#'  \item \code{ends_with(x, ignore.case = FALSE)}:
#'    names ends in \code{x}
#'  \item \code{contains(x, ignore.case = FALSE)}:
#'    selects all variables whose name contains \code{x}
#'  \item \code{matches(x, ignore.case = FALSE)}:
#'    selects all variables whose name matches the regular expression \code{x}
#'  \item \code{num_range("x", 1:5, width = 2)}:
#'    selects all variables (numerically) from x01 to x05.
#' }
#'
#' To drop variables, use \code{-}. You can rename variables with
#' named arguments.
#' @rdname manip
#' @export
#' @examples
#' # More detailed select examples ------------------------------
#' iris <- tbl_df(iris) # so it prints a little nicer
#' select(iris, starts_with("Petal"))
#' select(iris, ends_with("Width"))
#' select(iris, contains("etal"))
#' select(iris, matches(".t."))
#' select(iris, Petal.Length, Petal.Width)
#'
#' df <- as.data.frame(matrix(runif(100), nrow = 10))
#' df <- tbl_df(df[c(3, 4, 7, 1, 9, 8, 5, 2, 6, 10)])
#' select(df, V4:V6)
#' select(df, num_range("V", 4:6))
#'
#' # Drop variables
#' select(iris, -starts_with("Petal"))
#' select(iris, -ends_with("Width"))
#' select(iris, -contains("etal"))
#' select(iris, -matches(".t."))
#' select(iris, -Petal.Length, -Petal.Width)
#'
#' # Rename variables
#' select(iris, petal_length = Petal.Length)
#' select(iris, petal = starts_with("Petal"))
select <- function(.data, ...) UseMethod("select")

#' The number of observations in the current group.
#'
#' This function is implemented special for each data source and can only
#' be used from within \code{\link{summarise}}.
#'
#' @export
#' @examples
#' if (require("hflights")) {
#' carriers <- group_by(hflights, UniqueCarrier)
#' summarise(carriers, n())
#' }
n <- function() {
  stop("This function should not be called directly")
}
