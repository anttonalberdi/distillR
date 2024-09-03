#' Compute the shortest paths
#'
#' @return
#' @export
#'
#' @examples
distill <- function(dram_annotations) {

  dram_annotations %>%
    dplyr::select(mag_id, annotation_id) %>%
    dplyr::distinct() %>%
    dplyr::group_by(mag_id) %>%
    dplyr::summarise(
      annotation_ids = list(annotation_id)
    ) %>%
    dplyr::mutate(
      bundle_cost = get_bundle_cost(
        annotation_vector = annotation_ids
      ) %>% list()
    ) %>%
    dplyr::select(-annotation_ids) %>%
    tidyr::unnest(bundle_cost)

}
