#' Generates a gene presence-based genome-Inferred Functional Trait (GIFT) table
#'  at the gene bundle level from a bacterial genome annotation table
#'
#' @param annotation_table Table containing genome identifiers and gene
#' annotations
#' @param giftdb Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @param genomecol Column index (number) of the annotation_table containing the
#' genome identifiers
#' @param annotcol Column index(es) of the annotation_table in which to search
#' for gene identifiers (e.g., c(3,4,5))
#' @importFrom stringr str_extract
#' @return A gene bundle-level GIFT table
#' @examples
#' distill(
#'     distillR::gene_annotations,
#'     distillR::GIFT_db,
#'     genomecol = 2,
#'     annotcol = c(9, 10, 19)
#' )
#' @export

distill <- function(
    annotation_table,
    giftdb,
    genomecol = 2,
    annotcol = c(9, 10, 19)
  ) {

  annotation_table <- as.data.frame(annotation_table)
  giftdb <- as.data.frame(giftdb)

  if (!missing(genomecol)) {
    genomes <- unique(annotation_table[, genomecol])
  } else {
    genomes <- "GIFT"
  }

  gift_table <- compose_gift_table(
    genomes,
    annotation_table, genomecol, annotcol,
    giftdb
  )

  rownames(gift_table) <- genomes
  colnames(gift_table) <- giftdb$Code_bundle
  gift_table[is.na(gift_table)] <- 0

  return(gift_table)
}


compose_identifier_vector <- function(annotations_genome, annotcol) {
  identifier_vector <-
    annotations_genome[, annotcol] %>%  # annotcol is a number, not a name
    c() %>%
    unlist() %>%
    c() %>%
    stringr::str_extract(  # Parse identifiers (KEGG|EC)
      pattern = "K[0-9]{5}|(?<=\\[EC:).+?(?=\\])"
    ) %>%
    unique() %>% # Dereplicate
    stats::na.exclude() %>%
    stringr::str_split(pattern = " ") %>%
    unlist() %>%
    .[!grepl(pattern = "-", x = ., fixed = TRUE)] # Remove ambiguous EC codes  # nolint

  return(identifier_vector)

}


compose_gift_vector <- function(identifier_vector, giftdb = distillR::GIFT_db) {
  gift_list <- list()
  for (row_id in seq_len(nrow(giftdb))) {
    definition <- giftdb[row_id, "Definition"]
    gift_list[[row_id]] <- compute_gift(definition, identifier_vector)
  }
  gift_vector <- as.numeric(gift_list)

  return(gift_vector)
}


compose_gift_table <- function(
    genomes,
    annotation_table, genomecol, annotcol,
    giftdb
  ) {
  gift_table <- c()
  m <- 0
  for (genome in genomes) {
    m <- m + 1
    if (length(genomes) > 1) {
      annotations_genome <-
        annotation_table[annotation_table[, genomecol] == genome, ]
    } else {
      annotations_genome <- annotation_table
    }

    identifier_vector <- compose_identifier_vector(annotations_genome, annotcol)

    gift_vector <- compose_gift_vector(identifier_vector, giftdb)
    gift_table <- rbind(gift_table, gift_vector)
  }

  return(gift_table)
}
