context("test-team_5")

test_that("error message works", {
  expect_error(team_5("Where is the file", "0.1"), "The argument, tolerance, must be a single number")
  #I am not sure how to make a test for public since they don't have my data at the same local directory as mine.
})
