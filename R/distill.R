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
    annotation_table, giftdb,
    genomecol = 2,
    annotcol = c(9, 10, 19)
  ) {

  annotation_table <- as.data.frame(annotation_table)
  giftdb <- as.data.frame(giftdb)

  # List genomes
  if (!missing(genomecol)) {
    genomes <- unique(annotation_table[, genomecol])
  } else {
    genomes <- "GIFT"
  }

  # Calculate GIFTs for each genome iteratively
  gift_table <- c()
  m <- 0
  for (genome in genomes) {
    m <- m + 1
    if (length(genomes) > 1) {
      cat("\t", genome, " (", m, "/", length(genomes), ")\n", sep = "")
      # Fetch genome annotations
      annotations_genome <-
        annotation_table[annotation_table[, genomecol] == genome, ]
    } else {
      annotations_genome <-
        annotation_table
    }

    # Create vector of identifiers
    identifier_vector <-
      c(unlist(c(annotations_genome[, annotcol]))) %>%
      stringr::str_extract(  # Parse identifiers (KEGG|EC)
        pattern = "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])"
      ) %>%
      unique() %>% # Dereplicate
      stats::na.exclude() %>%
      stringr::str_split(pattern = " ") %>%
      unlist() %>%
      .[!grepl(pattern = "-", x = ., fixed = TRUE)] # Remove ambiguous EC codes  # nolint

    # Calculate GIFTs for each Pathway and append to vector
    gift_vector <- c()
    for (row_id in seq_len(nrow(giftdb))) {
      definition <- giftdb[row_id, "Definition"]
      gift <- compute_gift(definition, identifier_vector)
      gift_vector <- c(gift_vector, gift)
    }
    # Append GIFT vector of the genome to the GIFT table containing GIFT values
    # of all genomes
    gift_table <- rbind(gift_table, gift_vector)
  }

  # Format output GIFT table
  rownames(gift_table) <- genomes
  colnames(gift_table) <- giftdb$Code_bundle
  gift_table[is.na(gift_table)] <- 0

  # Output GIFT table
  return(gift_table)
}
