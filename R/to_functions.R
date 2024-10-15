#' Aggregate function-level GIFTs
#'
#' @param distilled tibble result from distill
#'
#' @return tibble with aggregated function-level GIFTs
#' @export
#'

to_functions <- function(distilled) {
  distilled |>
    dplyr::left_join(gift_info, by = "pathway_id") |>
    dplyr::select(
      mag_id, function_id, function_name, pathway_id, length_shortest_path, cost
    ) |>
    dplyr::distinct() |>
    dplyr::group_by(mag_id, function_id, function_name) |>
    dplyr::summarise(
      length_shortest_path = sum(length_shortest_path),
      cost = sum(cost)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      completeness = 1 - (cost / length_shortest_path),
    )
}
