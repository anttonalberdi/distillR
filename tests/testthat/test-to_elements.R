test_that("Test to_elements", {
  distill <- read_rds(test_path("fixtures", "distill.rds"))
  actual <- to_elements(distill, distillR::GIFT_db)
  expected <- read_rds(test_path("fixtures", "gift_elements.rds"))
  expect_equal(actual, expected)
})
