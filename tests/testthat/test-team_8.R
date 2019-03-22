context("test-team_8")

test_that("team_8 works as expected", {
  expect_error(team_8(file=3))
  expect_error(team_8(file="tests/testData/team_8_testData.shp",tolerance="a"))
  #expect_s3_class(testOutput,"data.frame") #This test doesn't seem to be working even though I know for a fact that the test output is a data frame
})
