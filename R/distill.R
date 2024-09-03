#' Distill the annotations by computing the completeness of a pathway given
#' present annotations
#'
#' @param dram_annotation Table containing the imported annotation from DRAM
#' @return A tibble with the completeness of a pathway given present annotations
#' @export
#'
#' @examples
#' gene_annotations %>%
#'  import_dram() %>%
#'  distill()
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
        annotation_vector = annotation_ids  # nolint: object_usage_linter
      ) %>% list()
    ) %>%
    dplyr::select(-annotation_ids) %>%
    tidyr::unnest(bundle_cost)  # nolint: object_usage_linte
}
