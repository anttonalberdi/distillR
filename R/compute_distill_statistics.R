#' Compute the statistics from the distilled genomes
#'
#' @param identifier_vector character vector with the KEGG, EC and peptidase
#' identifiers
#' @param giftdb Table with the GIFTs (defaults to distillR::GIFT_DB)
#'
#' @return A tibble with the statistics
#' @export
#'
#' @examples
#' identifiers <- c("K01580", "K00823", "K16871")
#' compute_distill_statistics(identifiers)
compute_distill_statistics <- function(
    identifier_vector, giftdb = distillR::GIFT_db) {
  db_identifiers <-
    giftdb$Definition %>%
    paste(collapse = " ") %>%
    strsplit(split = " |\\,|\\)|\\(|\\+") %>%
    unlist() %>%
    unique()
  length_db <- length(db_identifiers)
  length_data <- length(identifier_vector)
  length_intersect <- length(intersect(db_identifiers, identifier_vector))

  statistics <- tibble(
    statistic = c(
      "identifiers_in_table", "identifiers_in_database", "identifiers_in_both",
      "ratio_table_used", "ratio_db_used"
    ),
    values = c(
      length_data, length_db, length_intersect,
      round(length_intersect / length_data * 100, 2),
      round(length_intersect / length_db * 100, 2)
    )
  )

  return(statistics)
}
