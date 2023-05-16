test_that("Test create_step_matrix", {
  definition_decomposed <- c(
    "K01580", " ", "(", "K13524", ",", "K07250", ",", "K00823", ",", "K16871",
    ")", " ", "(", "K00135", ",", "(", "K00139", ",", "K17761", ")", ")"
  )
  definition_levels <-
    c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 2, 1, 0)
  actual <- create_step_matrix(definition_decomposed, definition_levels)
  expected <-
    data.frame(
      def_decomp = c(
        "K01580", " ", NA, "K13524", ",", "K07250", ",", "K00823", ",", "K16871",
        NA, " ", NA, "K00135", ",", NA, "K00139", ",", "K17761", NA, NA
      ),
      def_level = c( # Strings, yes
        "0", "0", NA, "1", "1", "1", "1", "1", "1", "1", NA, "0", NA, "1", "1",
        NA, "2", "2", "2", NA, NA
      ),
      L0_group = c(
        1, 2, NA, 2, 2, 2, 2, 2, 2, 2, NA, 3, NA, 3, 3, NA, 3, 3, 3, NA, NA
      ),
      L1_group = c(
        NA, NA, NA, 1, 2, 2, 3, 3, 4, 4, NA, NA, NA, 1, 2, NA, 2, 2, 2, NA, NA
      ),
      L2_group = c(
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2,
        NA, NA
      )
    )
  expect_equal(actual, expected)
})
