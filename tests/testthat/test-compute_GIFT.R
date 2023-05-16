test_that("multiplication works", {
  actual <- compute_GIFT(
    definition = "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))",
    present = c("K01580", "K00823", "K16871")
  )
  expected <- 0.67
  expect_equal(actual, expected)
})
