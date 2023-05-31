test_that("Test compute_gift", {
  definition <- readr::read_rds(test_path("fixtures", "definition.rds"))
  present <- readr::read_rds(test_path("fixtures", "present.rds"))
  actual <- compute_gift(definition = definition, present = present)
  expected <- 0.67
  expect_equal(actual, expected)
})
