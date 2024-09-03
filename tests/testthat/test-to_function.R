test_that("to_function returns something", {
  distilled <- readr::read_rds(test_path("fixtures", "distill_expected.rds"))
  functions <- to_function(distilled)
})
