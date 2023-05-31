test_that("Test to_elements", {
  gift_table <- readr::read_rds(test_path("fixtures", "gift_table.rds"))
  actual <- to_elements(gift_table = gift_table, gift_db = distillR::GIFT_db)
  expected <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
  expect_equal(actual, expected)
})
