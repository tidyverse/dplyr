#' ---
#' output: github_document
#' ---
#'
#+ setup, include = FALSE, cache = FALSE
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)

#+ body
library(googlesheets4)
library(cranlogs)
library(tidyverse)
library(here)

ssid <- "1m1u4f8PidRko2KGcXaz6BOYOcXREdr4Twc0zig9AEb8"
sheets_auth(email = "jenny@rstudio.com")
# sheets_browse(ssid)
dat <- sheets_read(ssid)

dl_raw <- cran_downloads(when = "last-month", packages = dat$Package)

dl <- dl_raw %>%
  as_tibble() %>%
  count(package, wt = count) %>%
  arrange(package) %>%
  mutate(
    prop = n / sum(n),
    rank = min_rank(desc(n))
  )

# don't do this casually
#sheets_write(dl, ss = ssid, sheet = "downloads")

# NOTE: I then did assorted "hand work" in the actual Google Sheet:
#   * protected the 'downloads' sheet
#   * created 'downloads_n' named range as 'downloads!B2:B293'
#   * replaced static 'prop' column with '=B2/sum(downloads_n)'
#   * formatted 'prop' column as percentage with format string '0.0%'
#   * replaced static 'rank' column with '=rank(B2,downloads_n)'
#   * added 'dl_summary' column with
#     '=CONCATENATE(TO_TEXT(D2), " (", to_text(C2), ")")'
#   * created 'downloads_table' named range as 'downloads!A:E'
#   * inserted new column 'Downloads' into main table with
#     '=VLOOKUP(A3,downloads_table,5,FALSE)'
#   * protected the 'Downloads' column
#
# In hindsight, many of the formula columns above could have been created
# in R and included in the main write.

dl %>%
  arrange(desc(n)) %>%
  mutate(cum_prop = cumsum(prop)) %>%
  print(n = 20)

write_csv(dl, here("revdep", "revep-downloads.csv"))
