test_that("Check that set_levels work", {
  definition_decomposed <- c(
    "K01580", " ", "(", "K13524", ",", "K07250", ",", "K00823", ",", "K16871",
    ")", " ", "(", "K00135", ",", "(", "K00139", ",", "K17761", ")", ")"
  )
  actual <- set_levels(definition_decomposed)
  expected <- c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 1, 2, 2, 2, 2, 1, 0)
  expect_equal(actual, expected)
})
