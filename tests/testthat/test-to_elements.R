test_that("multiplication works", {
  distill <- read_rds(test_path("fixtures", "distill.rds"))
  actual <- to.elements(distill, distillR::GIFT_db)
  expected <- read_rds(test_path("fixtures", "gift_elements.rds"))
  expect_equal(actual, expected)
})
