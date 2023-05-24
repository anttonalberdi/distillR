test_that("multiplication works", {
  elements <- read_rds(test_path("fixtures", "gift_elements.rds"))
  actual <- to.functions(elements, distillR::GIFT_db)
  # write_rds(actual, test_path("fixtures", "gift_functions.rds"))
  expected <- read_rds(test_path("fixtures", "gift_functions.rds"))
  expect_equal(actual, expected)
})
