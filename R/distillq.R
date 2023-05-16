#' Generates a list of gene count-based Genome-Inferred Functional Trait (GIFT) tables at the gene bundle level from bacterial genome annotation and gene count tables
#'
#' @param gene_count_table Table containing normalised gene expression data with gene identifiers in rows and sample identifiers in columns
#' @param annotation_table Table containing gene and genome identifiers, and gene annotations
#' @param GIFT_db Table containing definitions and metadata of metabolic functions (provided by DAMMA)
#' @param genecol Column index (number) of the annotations table containing the gene identifiers
#' @param genomecol Column index (number) of annotation_table containing the genome identifiers
#' @param annotcol Column index(es) of the annotation_table in which to search for gene identifiers (e.g., c(3,4,5))
#' @import tidyverse
#' @importFrom stringr str_extract str_match_all str_count
#' @importFrom reshape2 colsplit
#' @return A list of quantitative GIFT tables (one table per genome)
#' @examples
#' distillq(gene_count_table, annotation_table, GIFT_db, genecol, genomecol, keggcol, eccol, pepcol)
#' distillq(gene_count_table, annotation_table, GIFT_db, genecol = 1, genomecol = 2, keggcol = 9, eccol = c(10, 19), pepcol = 12)
#' @export

# UNDER DEVELOPMENT
distillq <- function(gene_count_table, annotation_table, GIFT_db, genecol = 1, genomecol = 2, annotcol = c(9, 10, 19)) {
# Sanity check
  if (missing(gene_count_table)) stop("Gene expression table is missing")
  if (missing(annotation_table)) stop("Genome annotation table is missing")
  if (missing(GIFT_db)) stop("GIFT database is missing")
  if (missing(genecol)) stop("Specify a column containing Gene identifiers")
  if (missing(genomecol)) stop("Specify a column containing Genome identifiers")
  if (length(genecol) != 1) stop("The argument genecol must be an integer indicating the number of the column containing the Gene identifiers in the annotations table")
  if (length(genomecol) != 1) stop("The argument genomecol must be an integer indicating the number of the column containing the Genome identifiers in the annotations table")
  if (missing(annotcol)) stop("Specify at least one column containing functional annotations")

  cat("Starting distillR quantitative analysis\n(Note this may take a while)...\n")

# Convert to tibble, filter, simplify to a three-column tibble, and extract identifiers

  intersect <- intersect(gene_count_table[[1]], annotation_table[[genecol]]) # list overlapping genes

  cat("\tParsing identifiers from annotation table...\n")
  annotation_table <- annotation_table %>%
    as_tibble() %>%
    filter(.[[genecol]] %in% intersect) %>% # filter overlapping genes
    select(c(genecol, genomecol, annotcol)) %>% # select relevant columns
# Process annotations
    unite("Annotation", 3:ncol(.), remove = T, sep = " ") %>% # paste annotation columns into a single column
    mutate(Annotation = str_extract_all(Annotation, "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])")) %>% # Extract identifiers
    rowwise() %>%
    mutate(Annotation = list(unlist(strsplit(Annotation, " ")))) %>% # Separate multiple EC annotations (e.g. "4.3.2.5.7 4.5.2.34" into "4.3.2.5.7","4.5.2.34")
    ungroup() %>%
    rename(Gene = 1, Genome = 2) # rename columns

  gene_count_table <- gene_count_table %>%
    as_tibble() %>%
    filter(.[[1]] %in% intersect) %>% # filter overlapping genes
    rename(Gene = 1) # rename gene column

# Merge annotations and count data
  annotation_gene_count_table <- inner_join(annotation_table, gene_count_table, by = "Gene")

# List Genomes
  Genomes <- unique(annotation_gene_count_table$Genome)

# Calculate expression values for each Genome
  if (ncol(gene_count_table) > 2) {
    cat("\tCalculating gene count-based GIFTs for", length(Genomes), "genomes and", ncol(gene_count_table) - 1, "samples...\n")
  } else {
    cat("\tCalculating gene count-based GIFTs for", length(Genomes), "genomes and", ncol(gene_count_table) - 1, "sample...\n")
  }
  m <- 0
  qGIFT_table_list <- list()
  for (Genome in Genomes) {
    m <- m + 1
    cat("\t", Genome, " (", m, "/", length(Genomes), ")\n", sep = "")

# Subset per Genome
    annotation_gene_count_filt <- annotation_gene_count_table %>%
      filter(Genome == Genomes[m]) %>% # filter annotations corresponding to the genome
      filter(!map_lgl(Annotation, is.null)) %>% # remove genes without annotations
      select(-c(1, 2)) # remove gene and genome columns

    if (nrow(annotation_gene_count_filt) > 0) {
      suppressWarnings(
        for (f in c(1:nrow(GIFT_db))) {
          definition <- GIFT_db[f, "Definition"]
          qGIFT <- compute_qGIFT(definition, annotation_gene_count_filt)
          if (f == 1) {
# Create list if it is the first function
            qGIFT_list <- qGIFT
          } else {
# Append to list if it is not the first function
            qGIFT_list <- Map(c, qGIFT_list, qGIFT)
          }
        }
      )
# Convert sample list to matrix
      qGIFT_list <- lapply(qGIFT_list, function(x) as.numeric(x))
      qGIFT_table <- do.call(rbind, qGIFT_list)
      colnames(qGIFT_table) <- GIFT_db$Code_bundle

# Append to Genome list
      qGIFT_table_list[[Genome]] <- qGIFT_table
    }
  }

  return(qGIFT_table_list)
}
