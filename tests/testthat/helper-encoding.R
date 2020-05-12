get_lang_strings <- function() {
  lang_strings <- c(
    de = "Gl\u00fcck",
    cn = "\u5e78\u798f",
    ru = "\u0441\u0447\u0430\u0441\u0442\u044c\u0435",
    ko = "\ud589\ubcf5"
  )

  native_lang_strings <- enc2native(lang_strings)

  same <- (lang_strings == native_lang_strings)

  list(
    same = lang_strings[same],
    different = lang_strings[!same]
  )
}

get_native_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$same) == 0) testthat::skip("No native language string available")
  lang_strings$same[[1L]]
}

get_alien_lang_string <- function() {
  lang_strings <- get_lang_strings()
  if (length(lang_strings$different) == 0) testthat::skip("No alien language string available")
  lang_strings$different[[1L]]
}

try_encoding <- function(enc) {
  orig_encoding <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", orig_encoding), add = TRUE)
  tryCatch({
    Sys.setlocale("LC_CTYPE", enc)
    TRUE
  },
  warning = function(w) FALSE,
  error = function(e) FALSE
  )
}

non_utf8_encoding <- function(enc = NULL) {
  if (!l10n_info()$`UTF-8`) {
    return(Sys.getlocale("LC_CTYPE"))
  }
  enc <- enc %||% c(
    "en_US.ISO8859-1",
    "en_US.ISO8859-15",
    "fr_CH.ISO8859-1",
    "fr_CH.ISO8859-15"
  )
  available <- vapply(enc, try_encoding, logical(1))
  if (any(available)) {
    enc[available][1]
  } else {
    NULL
  }
}

local_non_utf8_encoding <- function(enc = NULL, env = parent.frame()) {
  non_utf8 <- non_utf8_encoding(enc)
  if (is.null(non_utf8)) {
    skip("Can't set a non-UTF-8 encoding")
  } else {
    withr::local_locale(c(LC_CTYPE = non_utf8), .local_envir = env)
  }
}
