#' Aggregate function-level GIFTs
#'
#' @param distilled tibble result from distill
#'
#' @return tibble with aggregated function-level GIFTs
#' @export
#'
#' @examples
#' gene_annotations %>%
#'   import_dram() %>%
#'   distill() %>%
#'   to_function()
to_function <- function(distilled) {
  distilled %>%
    dplyr::left_join(gift_df) %>%
    dplyr::select(
      mag_id, function_id, function_name, pathway_id, length_shortest_path, cost  # nolint object_usage_linter
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(mag_id, function_id, function_name) %>%  # nolint object_usage_linter
    dplyr::summarise(
      length_shortest_path = sum(length_shortest_path),
      cost = sum(cost)
    ) %>%
    dplyr::mutate(
      completeness = 1 - (cost / length_shortest_path),
    )
}
