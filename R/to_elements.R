#' Aggregate element-level GIFTs
#'
#' @param distilled tibble result from distill
#'
#' @return tibble with aggregated domain-level GIFTs
#' @export
#'

to_elements <- function(distilled) {
  distilled %>%
    dplyr::left_join(gift_df, by = "pathway_id") %>%
    dplyr::select(
      mag_id, element_id, element_name, pathway_id, length_shortest_path, cost # nolint object_usage_linter
    ) %>%
    dplyr::distinct() %>%
    dplyr::summarise(
      length_shortest_path = sum(length_shortest_path),
      cost = sum(cost),
      .by = c(mag_id, element_id, element_name) # nolint object_usage_linter
    ) %>%
    dplyr::mutate(
      completeness = 1 - (cost / length_shortest_path),
    )
}
