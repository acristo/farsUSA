test_that("file name making works", {
  year <- 2011
  t.filename <- make_filename(year)
  expect_that(typeof(t.filename), is_a("character"))
  })
