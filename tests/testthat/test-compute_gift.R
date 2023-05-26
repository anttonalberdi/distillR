test_that("Test compute_gift", {
  definition <- read_rds(test_path("fixtures", "definition.rds"))
  present <- read_rds(test_path("fixtures", "present.rds"))
  actual <- compute_gift(definition = definition, present = present)
  expected <- 0.67
  expect_equal(actual, expected)
})
