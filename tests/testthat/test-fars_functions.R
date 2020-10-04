test_that("fars_read() works as expected", {
  expect_is(fars_read("accident_2015.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2017.csv.bz2"))
})

test_that("fars_summarize_years() works as expected", {
  expect_is(fars_summarize_years(2013:2015), "tbl_df")
  expect_error(fars_summarize_years(2017))
})

test_that("fars_map_state() works as expected", {
  expect_silent(fars_map_state(1, 2015))
  expect_error(fars_map_state(1, 2017))
})
