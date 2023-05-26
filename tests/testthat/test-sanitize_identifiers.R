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
