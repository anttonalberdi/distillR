test_that("Test distill", {
  actual <- read_rds(test_path("fixtures", "gift_table.rds"))
  expected <- distill(
    annotation_table =
      distillR::gene_annotations %>% filter(genome %in% c("MAG1", "MAG2")),
    giftdb = distillR::GIFT_db,
    genomecol = 2,
    annotcol = c(9, 10, 19)
  )
  expect_equal(actual, expected)
})
