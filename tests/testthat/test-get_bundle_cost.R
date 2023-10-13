test_that("get_bundle_cost returns something", {
  anntation_vec <- c("S09X", "6.1.1.4")
  expect_visible(get_bundle_cost(anntation_vec))
})
