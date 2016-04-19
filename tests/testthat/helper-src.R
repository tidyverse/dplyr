test_register_src("df", src_df(env = new.env(parent = emptyenv())))
test_register_src("sqlite", src_sqlite(":memory:", create = TRUE))

if (identical(Sys.info()[["user"]], "hadley")) {
  test_register_src("postgres", src_postgres("test", host = "localhost"))
} else if (identical(Sys.getenv("TRAVIS"), "true")) {
  test_register_src("postgres", src_postgres("test", user = "travis", password = ""))
}



skip_if_no_sqlite <- function() {
  if (!test_srcs$has("sqlite"))
    skip("No SQLite")
}
