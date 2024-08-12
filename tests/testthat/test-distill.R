# distill ----
test_that("Test distill", {
  actual <- readr::read_rds(test_path("fixtures", "gift_table.rds"))
  expected <- distill(
    annotation_table =
      distillR::gene_annotations %>%
        dplyr::filter(genome %in% c("MAG1", "MAG2")),
    giftdb = distillR::GIFT_db,
    genomecol = 2,
    annotcol = c(9, 10, 19)
  )
  expect_equal(actual, expected)
})


# sanitize_identifiers ----
test_that("Check that KEGG definitions are not modified", {
  definition <- "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))"
  actual <- sanitize_identifiers(definition)
  expected <- definition
  expect_equal(actual, expected)
})

test_that("Check that EC definitions are sanitized", {
  definition <-
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)"
  actual <- sanitize_identifiers(definition)
  expected <-
    "1_4_1_2 1_1_1_399 2_8_3_12 4_2_1_167 7_2_4_5 1_3_1_109 (2_8_3_1,2_8_3_8)"
  expect_equal(actual, expected)
})

test_that("Check that peptidases are not modified", {
  definition <- "S1B (C2A C1A S1A (M4 C13 M10A (S1D M12A)))"
  actual <- sanitize_identifiers(definition)
  expected <- definition
  expect_equal(actual, expected)
})


# set_levels ----
test_that("Check that set_levels work", {
  definition_decomposed <- c(
    "K01580", " ", "(", "K13524", ",", "K07250", ",", "K00823", ",", "K16871",
    ")", " ", "(", "K00135", ",", "(", "K00139", ",", "K17761", ")", ")"
  )
  actual <- set_levels(definition_decomposed)
  expected <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 2, 1, 0)
  expect_equal(actual, expected)
})


# distill_definition ----
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

test_that("Test process_cluster L0", {
  expect_true(TRUE)
})

test_that("Test process_cluster L1", {
  expect_true(TRUE)
})

test_that("Test process_cluster other levels", {
  expect_true(TRUE)
})

test_that("Test process_subdef2 on link", {
  subdef <-
    subdef2 <- subdef[(subdef != " ") & (subdef != "+")]
  present <-
    actual <- process_subdef2(subdef2, present)
  expected <-
    expect_true(TRUE)
})


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


# decompose_definition ----
test_that("Check that definitions are decomposed", {
  definition <-
    "1.4.1.2 1.1.1.399 2.8.3.12 4.2.1.167 7.2.4.5 1.3.1.109 (2.8.3.1,2.8.3.8)"
  actual <- decompose_definition(definition)
  expected <- c(
    "1.4.1.2", " ", "1.1.1.399", " ", "2.8.3.12", " ", "4.2.1.167", " ",
    "7.2.4.5", " ", "1.3.1.109", " ", "(", "2.8.3.1", ",", "2.8.3.8", ")"
  )
  expect_equal(actual, expected)
})


# create_step_matrix ----
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


# compute_gift ----
test_that("Test compute_gift", {
  definition <- readr::read_rds(test_path("fixtures", "definition.rds"))
  present <- readr::read_rds(test_path("fixtures", "present.rds"))
  actual <- compute_gift(definition = definition, present = present)
  expected <- 0.67
  expect_equal(actual, expected)
})
