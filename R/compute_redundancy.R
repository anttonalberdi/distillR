#' Compute the number of annotations present in a given pathway
#'
#' @param annotation_vector  A vector of annotations to compute the redundancy
#'
#' @return A tibble with the following columns:
#' - `pathway_id`: The pathway identifier
#' - `present`: The number of annotations present in the pathway
#' - `total`: The total number of annotations in the pathway
#' @export
#'
#' @examples
#' compute_redundancy(c("S09X", "6.1.1.4"))
compute_redundancy <- function(annotation_vector) {

  annotation_vector_clean <- annotation_vector |> unique()

  gift_graph |>
    dplyr::select(pathway_id, from, to) |>
    tidyr::pivot_longer(-pathway_id) |>
    tidyr::separate(value, into = c("module", "ko", "level"), sep = "_") |>
    dplyr::select(pathway_id, ko) |>
    dplyr::distinct() |>
    dplyr::filter(ko != "root") |>
    dplyr::summarise(
      ko_ids = list(ko),
      .by = pathway_id
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      present = sum(annotation_vector_clean %in% ko_ids),
      total = length(ko_ids)
    ) |>
    dplyr::select(pathway_id, present, total) |>
    dplyr::ungroup()

}
