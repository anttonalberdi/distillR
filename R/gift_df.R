#' GIFT pathways as a Tibble
#'
#'
#'
#' @format ## `gift_df`
#' A data frame with 27,489 rows and 29 columns, the result from DRAM. The most
#' important columns are
#' \describe{
#'   \item{gene}{The gene identifier}
#'   \item{genome}{The genome / MAG the gene belongs to}
#'   \item{kegg_id}{KEGG Ortholog identifiers}
#'   \item{kegg_hit}{KEGG description of the kegg_id}
#'   \item{peptidase_family}{Peptidase identifier}
#'   \item{cazy_hits}{CAZY hits that contain Enzymatic Codes}
#'
#' }
#' @source <https://github.com/anttonalberdi/distillR/data/gene_annotations.rds>
"gift_df"
