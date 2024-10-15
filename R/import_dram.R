#' Import DRAM table
#'
#' @param dram_raw_table Dataframe with the raw DRAM table.
#' Must contain at least the following columns:
#' - `...1`: gene name (DRAM leaves this column empty)
#' - `fasta`: MAG identifier
#' - `ko_id`: KEGG Ortholog IDs (KOs)
#' - `kegg_hit`: KEGG description with EC codes
#' - `cazy_ids`: CAZy family hits with EC codes
#' - `peptidase_family`: peptidase family identifiers
#'
#' From this table the KOs, EC and peptidase codes will be extracted for further
#' analyses for distillR.
#' @return Dataframe with the following columns:
#' - genome: provided genome names
#' - gene: provided gene names
#' - annotation_type: type of annotation extracted
#' @export
#'
#' @examples
#' dram |> import_dram()
import_dram <- function(dram_raw_table) {
  dram_raw_table |>
    tibble::as_tibble() |>
    dplyr::select(
      mag_id = fasta, gene_id = `...1`, ko_id, kegg_hit, cazy_ids, # nolint
      peptidase_family # nolint
    ) |>
    dplyr::group_by(mag_id) |>
    tidyr::pivot_longer(
      -mag_id:-gene_id,
      names_to = "annotation_type",
      values_to = "annotation"
    ) |>
    dplyr::filter(!is.na(annotation)) |>
    tidyr::pivot_wider(
      names_from = annotation_type,
      values_from = annotation
    ) |>
    tidyr::extract(
      col = kegg_hit,
      into = "kegg_hits",
      regex = "\\[EC:([0-9.\\- ]+)\\]",
      remove = FALSE
    ) |>
    dplyr::mutate(
      kegg_hits = stringr::str_split(kegg_hits, " "),
      cazy_ids = stringr::str_split(cazy_ids, "; ")
    ) |>
    tidyr::unnest(cazy_ids) |>
    tidyr::unnest(kegg_hits) |>
    dplyr::select(
      mag_id, gene_id, ko_id, kegg_hits, cazy_ids, peptidase_family
    ) |>
    tidyr::pivot_longer(
      -mag_id:-gene_id,
      names_to = "annotation_type",
      values_to = "annotation_id"
    ) |>
    dplyr::filter(!is.na(annotation_id)) |>
    dplyr::distinct() |>
    dplyr::ungroup()
}
