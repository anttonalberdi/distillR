test_that("multiplication works", {
  functions <- read_rds(test_path("fixtures", "gift_functions.rds"))
  actual <- to.domains(functions, distillR::GIFT_db)
  # write_rds(actual, test_path("fixtures", "gift_domains.rds"))
  expected <- read_rds(test_path("fixtures", "gift_domains.rds"))
  expect_equal(actual, expected)
})
