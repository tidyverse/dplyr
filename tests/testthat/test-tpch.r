context("tpch")

test_that("TPCH-R queries create correct results", {
	sf <- 0.1
	tbls <- tpchr::dbgen(sf=sf, lean=TRUE)
	s <- src_df(env = list2env(tbls))
	expect_true(tpchr::test_dplyr(s, 1, sf))
	expect_true(tpchr::test_dplyr(s, 2, sf))
	expect_true(tpchr::test_dplyr(s, 3, sf))
	expect_true(tpchr::test_dplyr(s, 4, sf))
	expect_true(tpchr::test_dplyr(s, 5, sf))
	expect_true(tpchr::test_dplyr(s, 6, sf))
	expect_true(tpchr::test_dplyr(s, 7, sf))
	expect_true(tpchr::test_dplyr(s, 8, sf))
	expect_true(tpchr::test_dplyr(s, 9, sf))
	expect_true(tpchr::test_dplyr(s, 10, sf))
})
