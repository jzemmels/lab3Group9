context("test-team_1")

test_that("team_1 works", {
  expect_error(team_1(1:3))
  expect_error(team_1(file="abc"))
  expect_error(team_1(file,tolerance="a"))

})
