# test_that("Test to_elements", {
#   gift_table <- readr::read_rds(test_path("fixtures", "gift_table.rds"))
#   actual <- to_elements(gift_table = gift_table, gift_db = distillR::GIFT_db)
#   expected <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
#   expect_equal(actual, expected)
# })

# test_that("Test to_functions", {
#   elements <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
#   actual <- to_functions(elements, distillR::GIFT_db)
#   # readr::write_rds(actual, test_path("fixtures", "gift_functions.rds"))  # nolint
#   expected <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
#   expect_equal(actual, expected)
# })

# test_that("Test to_domains", {
#   functions <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
#   actual <- to_domains(functions, distillR::GIFT_db)
#   # write_rds(actual, test_path("fixtures", "gift_domains.rds"))  # nolint
#   expected <- readr::read_rds(test_path("fixtures", "gift_domains.rds"))
#   expect_equal(actual, expected)
# })

# test_that("Test to_community from elements", {
#   elements <- readr::read_rds(test_path("fixtures", "gift_elements.rds"))
#   actual <- to_community(elements, distillR::genome_counts, distillR::GIFT_db)
#   # actual %>% write_rds(test_path("fixtures", "elements_community.rds"))  # nolint
#   expected <- readr::read_rds(test_path("fixtures", "elements_community.rds"))
#   expect_equal(actual, expected)
# })

# test_that("Test to_community from functions", {
#   functions <- readr::read_rds(test_path("fixtures", "gift_functions.rds"))
#   actual <- to_community(functions, distillR::genome_counts, distillR::GIFT_db)
#   # actual %>% write_rds(test_path("fixtures", "functions_community.rds"))  # nolint
#   expected <- readr::read_rds(test_path("fixtures", "functions_community.rds"))
#   expect_equal(actual, expected)
# })

# test_that("Test to_community from domains", {
#   domains <- readr::read_rds(test_path("fixtures", "gift_domains.rds"))
#   actual <- to_community(domains, distillR::genome_counts, distillR::GIFT_db)
#   # actual %>% write_rds(test_path("fixtures", "domains_community.rds"))  # nolint
#   expected <- readr::read_rds(test_path("fixtures", "domains_community.rds"))
#   expect_equal(actual, expected)
# })
