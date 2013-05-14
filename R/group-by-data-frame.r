
group_by.data.frame <- function(.data, ..., drop = TRUE) {
  name <- substitute(.data)
  splits <- lapply(named_dots(...), eval, .data, parent.frame())

  split_id <- id(splits, drop = drop)
  labels <- split_labels(splits, drop = drop, id = split_id)
  index <- split_indices(split_id, attr(split_id, "n"))

  structure(list(obj = .data, name = name, index = index, labels = labels),
    class = c("grouped_data_frame", "source_data_frame", "source"))
}

split_labels <- function(splits, drop, id = plyr::id(splits, drop = TRUE)) {
  if (length(splits) == 0) return(data.frame())

  if (drop) {
    # Need levels which occur in data
    representative <- which(!duplicated(id))[order(unique(id))]
    as_df(lapply(splits, function(x) x[representative]))
  } else {
    unique_values <- llply(splits, ulevels)
    names(unique_values) <- names(splits)
    rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE))
  }
}
