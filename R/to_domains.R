#' Aggregate domain-level GIFTs
#'
#' @param distilled tibble result from distill
#'
#' @return tibble with aggregated domain-level GIFTs
#' @export
#'

to_domains <- function(distilled) {

  distilled %>%
    dplyr::left_join(gift_info, by = "pathway_id") %>%
    dplyr::select(
      mag_id, domain_id, domain_name, pathway_id, length_shortest_path, cost # nolint object_usage_linter
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(mag_id, domain_id, domain_name) %>%
    dplyr::summarise(
      length_shortest_path = sum(length_shortest_path),
      cost = sum(cost)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      completeness = 1 - (cost / length_shortest_path),
    )
}
