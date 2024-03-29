% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join-by.R
\name{join_by}
\alias{join_by}
\alias{closest}
\alias{overlaps}
\alias{within}
\title{Join specifications}
\usage{
join_by(...)
}
\arguments{
\item{...}{Expressions specifying the join.

Each expression should consist of one of the following:
\itemize{
\item Equality condition: \code{==}
\item Inequality conditions: \code{>=}, \code{>}, \code{<=}, or \code{<}
\item Rolling helper: \code{closest()}
\item Overlap helpers: \code{between()}, \code{within()}, or \code{overlaps()}
}

Other expressions are not supported. If you need to perform a join on
a computed variable, e.g. \code{join_by(sales_date - 40 >= promo_date)},
you'll need to precompute and store it in a separate column.

Column names should be specified as quoted or unquoted names. By default,
the name on the left-hand side of a join condition refers to the left-hand
table, unless overridden by explicitly prefixing the column name with
either \verb{x$} or \verb{y$}.

If a single column name is provided without any join conditions, it is
interpreted as if that column name was duplicated on each side of \code{==},
i.e. \code{x} is interpreted as \code{x == x}.}
}
\description{
\code{join_by()} constructs a specification that describes how to join two tables
using a small domain specific language. The result can be supplied as the
\code{by} argument to any of the join functions (such as \code{\link[=left_join]{left_join()}}).
}
\section{Join types}{
The following types of joins are supported by dplyr:
\itemize{
\item Equality joins
\item Inequality joins
\item Rolling joins
\item Overlap joins
\item Cross joins
}

Equality, inequality, rolling, and overlap joins are discussed in more detail
below. Cross joins are implemented through \code{\link[=cross_join]{cross_join()}}.
\subsection{Equality joins}{

Equality joins require keys to be equal between one or more pairs of columns,
and are the most common type of join. To construct an equality join using
\code{join_by()}, supply two column names to join with separated by \code{==}.
Alternatively, supplying a single name will be interpreted as an equality
join between two columns of the same name. For example, \code{join_by(x)} is
equivalent to \code{join_by(x == x)}.
}

\subsection{Inequality joins}{

Inequality joins match on an inequality, such as \code{>}, \code{>=}, \code{<}, or \code{<=}, and
are common in time series analysis and genomics. To construct an inequality
join using \code{join_by()}, supply two column names separated by one of the above
mentioned inequalities.

Note that inequality joins will match a single row in \code{x} to a potentially
large number of rows in \code{y}. Be extra careful when constructing inequality
join specifications!
}

\subsection{Rolling joins}{

Rolling joins are a variant of inequality joins that limit the results
returned from an inequality join condition. They are useful for "rolling" the
closest match forward/backwards when there isn't an exact match. To construct
a rolling join, wrap an inequality with \code{closest()}.
\itemize{
\item \code{closest(expr)}

\code{expr} must be an inequality involving one of: \code{>}, \code{>=}, \code{<}, or \code{<=}.

For example, \code{closest(x >= y)} is interpreted as: For each value in \code{x},
find the closest value in \code{y} that is less than or equal to that \code{x} value.
}

\code{closest()} will always use the left-hand table (\code{x}) as the primary table,
and the right-hand table (\code{y}) as the one to find the closest match in,
regardless of how the inequality is specified. For example,
\code{closest(y$a >= x$b)} will always be interpreted as \code{closest(x$b <= y$a)}.
}

\subsection{Overlap joins}{

Overlap joins are a special case of inequality joins involving one or two
columns from the left-hand table \emph{overlapping} a range defined by two columns
from the right-hand table. There are three helpers that \code{join_by()}
recognizes to assist with constructing overlap joins, all of which can be
constructed from simpler inequalities.
\itemize{
\item \code{between(x, y_lower, y_upper, ..., bounds = "[]")}

For each value in \code{x}, this finds everywhere that value falls between
\verb{[y_lower, y_upper]}. Equivalent to \verb{x >= y_lower, x <= y_upper} by
default.

\code{bounds} can be one of \code{"[]"}, \code{"[)"}, \code{"(]"}, or
\code{"()"} to alter the inclusiveness of the lower and upper bounds. This
changes whether \code{>=} or \code{>} and \code{<=} or \code{<} are used to build the
inequalities shown above.

Dots are for future extensions and must be empty.
\item \code{within(x_lower, x_upper, y_lower, y_upper)}

For each range in \verb{[x_lower, x_upper]}, this finds everywhere that range
falls completely within \verb{[y_lower, y_upper]}. Equivalent to \verb{x_lower >= y_lower, x_upper <= y_upper}.

The inequalities used to build \code{within()} are the same regardless of the
inclusiveness of the supplied ranges.
\item \code{overlaps(x_lower, x_upper, y_lower, y_upper, ..., bounds = "[]")}

For each range in \verb{[x_lower, x_upper]}, this finds everywhere that range
overlaps \verb{[y_lower, y_upper]} in any capacity. Equivalent to \verb{x_lower <= y_upper, x_upper >= y_lower} by default.

\code{bounds} can be one of \code{"[]"}, \code{"[)"}, \code{"(]"}, or
\code{"()"} to alter the inclusiveness of the lower and upper bounds.
\code{"[]"} uses \code{<=} and \code{>=}, but the 3 other options use \code{<} and \code{>}
and generate the exact same inequalities.

Dots are for future extensions and must be empty.
}

These conditions assume that the ranges are well-formed and non-empty, i.e.
\code{x_lower <= x_upper} when bounds are treated as \code{"[]"}, and
\code{x_lower < x_upper} otherwise.
}
}

\section{Column referencing}{
When specifying join conditions, \code{join_by()} assumes that column names on the
left-hand side of the condition refer to the left-hand table (\code{x}), and names
on the right-hand side of the condition refer to the right-hand table (\code{y}).
Occasionally, it is clearer to be able to specify a right-hand table name on
the left-hand side of the condition, and vice versa. To support this, column
names can be prefixed by \verb{x$} or \verb{y$} to explicitly specify which table they
come from.
}

\examples{
sales <- tibble(
  id = c(1L, 1L, 1L, 2L, 2L),
  sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
)
sales

promos <- tibble(
  id = c(1L, 1L, 2L),
  promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02"))
)
promos

# Match `id` to `id`, and `sale_date` to `promo_date`
by <- join_by(id, sale_date == promo_date)
left_join(sales, promos, by)

# For each `sale_date` within a particular `id`,
# find all `promo_date`s that occurred before that particular sale
by <- join_by(id, sale_date >= promo_date)
left_join(sales, promos, by)

# For each `sale_date` within a particular `id`,
# find only the closest `promo_date` that occurred before that sale
by <- join_by(id, closest(sale_date >= promo_date))
left_join(sales, promos, by)

# If you want to disallow exact matching in rolling joins, use `>` rather
# than `>=`. Note that the promo on `2019-01-05` is no longer considered the
# closest match for the sale on the same date.
by <- join_by(id, closest(sale_date > promo_date))
left_join(sales, promos, by)

# Same as before, but also require that the promo had to occur at most 1
# day before the sale was made. We'll use a full join to see that id 2's
# promo on `2019-01-02` is no longer matched to the sale on `2019-01-04`.
sales <- mutate(sales, sale_date_lower = sale_date - 1)
by <- join_by(id, closest(sale_date >= promo_date), sale_date_lower <= promo_date)
full_join(sales, promos, by)

# ---------------------------------------------------------------------------

segments <- tibble(
  segment_id = 1:4,
  chromosome = c("chr1", "chr2", "chr2", "chr1"),
  start = c(140, 210, 380, 230),
  end = c(150, 240, 415, 280)
)
segments

reference <- tibble(
  reference_id = 1:4,
  chromosome = c("chr1", "chr1", "chr2", "chr2"),
  start = c(100, 200, 300, 415),
  end = c(150, 250, 399, 450)
)
reference

# Find every time a segment `start` falls between the reference
# `[start, end]` range.
by <- join_by(chromosome, between(start, start, end))
full_join(segments, reference, by)

# If you wanted the reference columns first, supply `reference` as `x`
# and `segments` as `y`, then explicitly refer to their columns using `x$`
# and `y$`.
by <- join_by(chromosome, between(y$start, x$start, x$end))
full_join(reference, segments, by)

# Find every time a segment falls completely within a reference.
# Sometimes using `x$` and `y$` makes your intentions clearer, even if they
# match the default behavior.
by <- join_by(chromosome, within(x$start, x$end, y$start, y$end))
inner_join(segments, reference, by)

# Find every time a segment overlaps a reference in any way.
by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end))
full_join(segments, reference, by)

# It is common to have right-open ranges with bounds like `[)`, which would
# mean an end value of `415` would no longer overlap a start value of `415`.
# Setting `bounds` allows you to compute overlaps with those kinds of ranges.
by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end, bounds = "[)"))
full_join(segments, reference, by)
}
