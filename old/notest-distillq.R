# distillq ----

# nolint start
# test_that("Test distillq", {
#   actual <- distillq(
#     gene_count_table = distillR::gene_expression,
#     annotation_table = distillR::gene_annotations,
#     GIFT_db = distillR::GIFT_db,
#     genecol = 1,
#     genomecol = 2,
#     annotcol = c(9,10,19)
#   )
#   expect_true(TRUE)
# })
# nolint end

distillq_definition - ---
  test_that("Test distillq_definition", {
    gene_count_table <- distillR::gene_expression
    annotation_table <- distillR::gene_annotations
    GIFT_db <- distillR::GIFT_db
    actual <- distillq(
      gene_count_table,
      annotation_table,
      GIFT_db,
      genecol = 1,
      genomecol = 2,
      annotcol = c(9, 10, 19)
    )
  })
