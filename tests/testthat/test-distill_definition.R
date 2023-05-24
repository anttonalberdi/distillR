test_that("Test distill_definition", {
  definition <- read_rds(test_path("fixtures", "definition.rds"))
  present <- read_rds(test_path("fixtures", "present.rds"))
  definition_table <- read_rds(test_path("fixtures", "definition_table.rds"))
  level <- "L2_group"
  actual <- distill_definition(definition, definition_table, level, present)
  expected <- definition
  expect_equal(actual, expected)
})
