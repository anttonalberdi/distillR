test_that("Test create_step_matrix", {
  definition_decomposed <- read_rds(test_path("fixtures", "definition_decomposed.rds"))
  definition_levels <- read_rds(test_path("fixtures", "definition_levels.rds"))
  actual <- create_step_matrix(definition_decomposed, definition_levels)
  expected <- read_rds(test_path("fixtures", "step_matrix.rds"))
  expect_equal(actual, expected)
})
