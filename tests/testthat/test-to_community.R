test_that("multiplication works", {
  elements <- read_rds(test_path("fixtures", "gift_elements.rds"))
  actual <- to.community(elements, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "elements_community.rds"))
  expected <- read_rds(test_path("fixtures", "elements_community.rds"))
  expect_equal(actual, expected)
})

test_that("multiplication works", {
  functions <- read_rds(test_path("fixtures", "gift_functions.rds"))
  actual <- to.community(functions, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "functions_community.rds"))
  expected <- read_rds(test_path("fixtures", "functions_community.rds"))
  expect_equal(actual, expected)
})

test_that("multiplication works", {
  domains <- read_rds(test_path("fixtures", "gift_domains.rds"))
  actual <- to.community(domains, distillR::genome_counts, distillR::GIFT_db)
  # actual %>% write_rds(test_path("fixtures", "domains_community.rds"))
  expected <- read_rds(test_path("fixtures", "domains_community.rds"))
  expect_equal(actual, expected)
})
