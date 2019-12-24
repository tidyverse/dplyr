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

with_non_utf8_encoding <- function(code) {
  old_encoding <- set_non_utf8_encoding()
  on.exit(set_encoding(old_encoding), add = TRUE)
  code
}

set_non_utf8_encoding <- function() {
  if (.Platform$OS.type == "windows") return(NULL)
  tryCatch(
    locale <- set_encoding("en_US.ISO88591"),
    warning = function(e) {
      tryCatch(
        locale <<- set_encoding("fr_CH.ISO8859-15"),
        warning = function(w) {
          testthat::skip("Cannot set latin-1 encoding")
        }
      )
    }
  )
  locale
}

set_encoding <- function(encoding) {
  if (is.null(encoding)) return(NULL)
  locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", encoding)
  locale
}
