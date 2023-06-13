test_that("Test distill_definition on L2_group", {
  definition <-
    readr::read_rds(test_path("fixtures", "definition.rds"))
  present <-
    readr::read_rds(test_path("fixtures", "present.rds"))
  definition_table <-
    readr::read_rds(test_path("fixtures", "definition_table.rds"))
  level <- "L2_group"
  actual <- distill_definition(definition, definition_table, level, present)
  expected <- definition
  expect_equal(actual, expected)
})

# test_that("Test process_cluster L0", {
# expect_true(TRUE)
# })

# test_that("Test process_cluster L1", {
# expect_true(TRUE)
# })

# test_that("Test process_cluster other levels", {
# expect_true(TRUE)
# })

# test_that("Test process_subdef2 on link", {
# subdef <-
# subdef2 <- subdef[(subdef != " ") & (subdef != "+")]
# present <-
# actual <- process_subdef2(subdef2, present)
# expected <-
# expect_true(TRUE)
# })


# test_that("Test get_value space or plus", {
# subdef <-
# present <-
# actual <-
# expected <-
# expect_equal(actual, expected)
# })

# test_that("Test get_value comma", {
# subdef <-
# present <-
# actual <-
# expected <-
# expect_equal(actual, expected)
# expect_true(TRUE)
# })
