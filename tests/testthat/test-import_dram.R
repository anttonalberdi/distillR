test_that("import_dram still returns the same tibble", {
  actual <- dram |> import_dram()
  # write_rds(actual, test_path("fixtures", "import_dram.rds"), compress = "xz", version = 2, compression = 9)  # nolint
  expected <- test_path("fixtures", "import_dram.rds") |> readr::read_rds()
  expect_equal(actual, expected)
})
