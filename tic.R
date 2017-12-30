add_package_checks()

get_stage("before_script") %>%
  add_code_step({
    dir.create("~/.R", showWarnings = FALSE)
    writeLines(
      "PKG_CXXFLAGS := ${PKG_CXXFLAGS} $${TRAVIS_CXXFLAGS}",
      "~/.R/Makevars"
    )
  })
