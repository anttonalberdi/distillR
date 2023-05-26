test_that("Test compute_GIFT", {
  definition <- read_rds(test_path("fixtures", "definition.rds"))
  present <- read_rds(test_path("fixtures", "present.rds"))
  actual <- compute_GIFT(definition = definition, present = present)
  expected <- 0.67
  expect_equal(actual, expected)
})
