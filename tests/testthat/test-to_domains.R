test_that("Test to_domains", {
  functions <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
  actual <- to_domains(functions, distillR::GIFT_db)
  # write_rds(actual, test_path("fixtures", "gift_domains.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "gift_domains.rds"))
  expect_equal(actual, expected)
})
