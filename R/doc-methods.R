# Adapted from sloop
methods_generic <- function(x) {
  # Return early if generic not defined in global environment
  # This happens when the documentation is read before the package is attached.
  if (!env_has(globalenv(), x, inherit = TRUE)) {
    return(data.frame())
  }

  info <- evalq(attr(utils::methods(x), "info"), envir = globalenv())
  info <- tibble::as_tibble(info, rownames = "method")

  generic_esc <- gsub("([.\\[])", "\\\\\\1", x)
  info$class <- gsub(paste0("^", generic_esc, "[.,]"), "", info$method)
  info$class <- gsub("-method$", "", info$class)
  info$source <- gsub(paste0(" for ", generic_esc), "", info$from)

  # Find package
  methods <- map2(
    info$generic, info$class,
    utils::getS3method,
    optional = TRUE,
    envir = globalenv()
  )
  envs <- map(methods, environment)
  info$package <- map_chr(envs, environmentName)

  # Find help topic, if it exists
  info$topic <- help_topic(info$method, info$package)

  info[c("generic", "class", "package", "topic", "visible", "source", "isS4")]
}

methods_rd <- function(x) {
  methods <- methods_generic(x)
  if (nrow(methods) == 0) {
    return("no methods found")
  }

  methods <- methods[order(methods$package, methods$class), , drop = FALSE]
  topics <- unname(split(methods, methods$package))
  by_package <- vapply(topics, function(x) {
    links <- topic_links(x$class, x$package, x$topic)
    paste0(x$package[[1]], " (", paste0(links, collapse = ", "), ")")
  }, character(1))

  paste0(by_package, collapse = ", ")
}

topic_links <- function(class, package, topic) {
  ifelse(is.na(topic),
    paste0("\\code{", class, "}"),
    paste0("\\code{\\link[", package, ":", topic, "]{", class, "}}")
  )
}

help_topic <- function(x, pkg) {
  find_one <- function(topic, pkg) {
    if (identical(pkg, "")) {
      return(NA)
    }

    path <- system.file("help", "aliases.rds", package = pkg)
    if (!file.exists(path)) {
      return(NA)
    }

    aliases <- readRDS(path)
    aliases[[topic]]
  }

  map2_chr(x, pkg, find_one)
}
