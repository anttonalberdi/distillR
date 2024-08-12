test_that("Compute distill statistics", {
  identifier_vector <- c("K01580", "K00823", "K16871")
  actual <- compute_distill_statistics(
    identifier_vector = identifier_vector,
    giftdb = distillR::GIFT_db
  )
  # write_rds(actual, test_path("fixtures", "distill_statistics.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "distill_statistics.rds"))
  expect_equal(actual, expected)
})
