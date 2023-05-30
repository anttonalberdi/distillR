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
#' @param stats Whether to calculate and print distillation statistics
#' @importFrom stringr str_extract
#' @return A gene bundle-level GIFT table
#' @examples
#' distill(annotation_table, giftdb, genomecol, annotcol, stats)
#' distill(annotation_table, giftdb, genomecol = 2, annotcol = c(9, 10, 19),
#' stats = TRUE)
#' @export

distill <- function(
    annotation_table, giftdb,
    genomecol = 2, annotcol = c(9, 10, 19)
  ) {
  # Sanity check
  if (missing(annotation_table))
    stop("genome annotation table is missing")
  if (missing(giftdb))
    stop("Pathway database is missing")
  if (length(genomecol) != 1)
    stop(
      "The argument genomecol must be an integer indicating the number of the ",
      "column containing the genome identifiers in the annotations table"
    )
  if (missing(annotcol))
    stop("Specify at least one column containing functional annotations")

  # Convert annotation table to data frame
  annotation_table <- as.data.frame(annotation_table)

  # Convert pathway database to data frame
  giftdb <- as.data.frame(giftdb)

  # List genomes
  if (!missing(genomecol)) {
    genomes <- unique(annotation_table[, genomecol])
  } else {
    genomes <- "GIFT"
  }

  # Verbosity
  if (length(genomes) > 1) {
    cat("Calculating GIFTs for", length(genomes), "genomes:\n")
  } else {
    cat("Calculating GIFTs for a single genome.\n")
    cat("\tNote: If you were expecting multiple genomes\n")
    cat("\tensure the genome identifier column is correctly specified.\n")
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
      str_extract(  # Parse identifiers (KEGG|EC)
        string = c(unlist(c(annotations_genome[, annotcol]))),
        pattern = "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])"
      ) %>%
      unique() %>% # Dereplicate
      na.exclude() %>%
      strsplit(split = " ") %>%
      unlist() %>%
      .[!grepl(pattern = "-", x = ., fixed = TRUE)] # Remove ambiguous EC codes  # nolint

    # Calculate GIFTs for each Pathway and append to vector
    gift_vector <- c()
    suppressWarnings(
      for (f in seq_len(nrow(giftdb))) {
        definition <- giftdb[f, "Definition"]
        gift <- compute_gift(definition, identifier_vector)
        gift_vector <- c(gift_vector, gift)
      }
    )
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
