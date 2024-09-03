#' Import DRAM table
#'
#' @param dram_raw_table Dataframe with the raw DRAM table.
#' Must contain at least the following columns:
#' - genome: genome name
#' - gene:
#' - kegg_id: KEGG Ortholog IDs (KOs)
#' - kegg_hit: KEGG description with EC codes
#' - cazy_hits: CAZy family hits with EC codes
#' - peptidase_family: peptidase family
#'
#' From this table the KOs, EC and peptidase codes will be exracted for further
#' analyses for distillR.
#' @return Dataframe with the following columns:
#' - genome: provided genome names
#' - gene: provided gene names
#' - annotation_type: type of annotation extracted
#' @export
#'
#' @examples
#' distillR::gene_annotations %>% import_dram()
import_dram <- function(dram_raw_table) {
  dram_raw_table %>%
    tibble::as_tibble() %>%
    dplyr::select(
      mag_id = genome, gene_id = gene, kegg_id, kegg_hit, cazy_hits,
      peptidase_family
    ) %>%
    dplyr::group_by(mag_id) %>%
    tidyr::pivot_longer(
      -mag_id:-gene_id,
      names_to = "annotation_type",
      values_to = "annotation"
    ) %>%
    dplyr::filter(annotation != "") %>%
    tidyr::pivot_wider(
      names_from = annotation_type,
      values_from = annotation
    ) %>%
    tidyr::extract(
      col = kegg_hit,
      into = "kegg_hits",
      regex = "\\[EC:([0-9.\\- ]+)\\]",
      remove = FALSE
    ) %>%
    dplyr::mutate(
      kegg_hits = stringr::str_split(kegg_hits, " "),
      cazy_hits = stringr::str_split(cazy_hits, "; ")
    ) %>%
    tidyr::unnest(cazy_hits) %>%
    tidyr::unnest(kegg_hits) %>%
    tidyr::extract(
      col = cazy_hits,
      into = c("cazy_hit"),
      regex = "EC ([0-9.\\- ]+)",
      remove = FALSE
    ) %>%
    dplyr::select(
      mag_id, gene_id, kegg_id, kegg_hits, cazy_hit, peptidase_family
    ) %>%
    tidyr::pivot_longer(
      -mag_id:-gene_id,
      names_to = "annotation_type",
      values_to = "annotation_id"
    ) %>%
    dplyr::filter(!is.na(annotation_id)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
}
