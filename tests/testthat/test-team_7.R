context("test-team_7")

test_that("team_7() works", {
  expect_error(team_1(1:3))
  expect_error(team_1(file="abc"))
  expect_error(team_1(file="./data/gadm36_AUS_shp/gadm36_AUS_1.shp",
                      tolerance="a"))

})
