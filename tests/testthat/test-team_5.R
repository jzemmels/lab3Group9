context("test-team_5")

test_that("error message works", {
  ozfilepath <- system.file("extdata/gadm36_AUS_1.shp", package = "lab3Group9")
  expect_error(team_5("inst/extdata/gadm36_AUS_1.shp", "0.1"), "The argument, tolerance, must be a single number")
  expect_equal(team_5(ozfilepath, 0.1)$country[1], "Australia")
  #I am not sure how to make a test for public since they don't have my data at the same local directory as mine.
})
