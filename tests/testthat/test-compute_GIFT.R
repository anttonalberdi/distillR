test_that("multiplication works", {
  actual <- compute_GIFT(
    definition = read_rds(test_path("fixtures", "definition.rds")),
    present = read_rds(test_path("fixtures", "present.rds"))
  )
  expected <- 0.67
  expect_equal(actual, expected)
})
