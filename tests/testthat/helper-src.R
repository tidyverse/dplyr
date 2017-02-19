if (test_srcs$length() == 0) {

  test_register_src("df", src_df(env = new.env(parent = emptyenv())))
  test_register_src("sqlite", src_sqlite(":memory:", create = TRUE))

  if (identical(Sys.getenv("TRAVIS"), "true")) {
    test_register_src("postgres", src_postgres("test", user = "travis", password = ""))
  } else {
    test_register_src("postgres", src_postgres("test", host = "localhost"))
  }

  test_register_src("mysql", src_mysql("test", host = "localhost", user = Sys.getenv("USER")))
}

skip_if_no_sqlite <- function() {
  if (!test_srcs$has("sqlite"))
    skip("No SQLite")
}
