test_that("Test distill_definition", {
  definition <- "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))"
  present <- c("K01580", "K00823", "K16871")
  definition_table <- data.frame(
    definition_decomposed = c(
      "K01580", " ", NA, "K13524", ",", "K07250", ",", "K00823", ",", "K16871",
      NA, " ", NA, "K00135", ",", NA, "K00139", ",", "K17761", NA, NA
    ),
    definition_levels = c( # Strings, yes
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
  level <- "L2_group"
  actual <- distill_definition(definition, definition_table, level, present)
  expected <- definition
  expect_equal(actual, expected)
})
