test_that("multiplication works", {
  elements <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
  actual <- to_community(elements, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "elements_community.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "elements_community.rds"))
  expect_equal(actual, expected)
})

test_that("multiplication works", {
  functions <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
  actual <- to_community(functions, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "functions_community.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "functions_community.rds"))
  expect_equal(actual, expected)
})

test_that("multiplication works", {
  domains <- readr::read_rds(test_path("fixtures", "gift_domains.rds"))
  actual <- to_community(domains, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "domains_community.rds"))  # nolint
  expected <- readr::read_rds(test_path("fixtures", "domains_community.rds"))
  expect_equal(actual, expected)
})
