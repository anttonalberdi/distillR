test_that("Test create_step_matrix", {
  definition_decomposed <-
    readr::read_rds(test_path("fixtures", "definition_decomposed.rds"))
  definition_levels <-
    readr::read_rds(test_path("fixtures", "definition_levels.rds"))
  actual <- create_step_matrix(definition_decomposed, definition_levels)
  # readr::write_rds(actual, test_path("fixtures", "step_matrix.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "step_matrix.rds"))
  expect_equal(actual, expected)
})
