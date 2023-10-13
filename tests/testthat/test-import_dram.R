test_that("Test import_dram returns something", {
  expect_visible(import_dram(gene_annotations))
})

test_that("Test import_dram still returns the same rows and columns", {
  annotations <- import_dram(gene_annotations)
  expect_equal(nrow(annotations), 30576)
  expect_equal(ncol(annotations), 4)
})
