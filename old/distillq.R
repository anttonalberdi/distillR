# distillq ----

#' Generates a list of gene count-based Genome-Inferred Functional Trait (GIFT)
#' tables at the gene bundle level from bacterial genome annotation and gene
#' count tables
#'
#' @param gene_count_table Table containing normalised gene expression data
#' with gene identifiers in rows and sample identifiers in columns
#' @param annotation_table Table containing gene and genome identifiers, and
#' gene annotations
#' @param GIFT_db Table containing definitions and metadata of metabolic
#' functions (provided by DAMMA)
#' @param genecol Column index (number) of the annotations table containing the
#' gene identifiers
#' @param genomecol Column index (number) of annotation_table containing the
#' genome identifiers
#' @param annotcol Column index(es) of the annotation_table in which to search
#' for gene identifiers (e.g., c(3,4,5))
#' @importFrom stringr str_extract str_match_all str_count
#' @importFrom reshape2 colsplit
#' @return A list of quantitative GIFT tables (one table per genome)
#' @examples
#' \dontrun{
#' distillq(
#'   gene_count_table, annotation_table, GIFT_db, genecol, genomecol,
#'   keggcol, eccol, pepcol
#' )
#' }
#' \dontrun{
#' distillq(gene_count_table, annotation_table, GIFT_db,
#'   genecol = 1,
#'   genomecol = 2, keggcol = 9, eccol = c(10, 19), pepcol = 12
#' )
#' }
#' @noexport

# UNDER DEVELOPMENT
# nolint start

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
    tidyr::unite( # paste annotation columns into a single column
      "Annotation",
      3:ncol(.),
      remove = TRUE,
      sep = " "
    ) %>%
    mutate( # Extract identifiers
      Annotation = stringr::str_extract_all(
        string = Annotation,
        pattern = "K[0-9][0-9][0-9][0-9][0-9]|(?<=\\[EC:).+?(?=\\])"
      )
    ) %>%
    rowwise() %>%
    mutate( # Separate multiple EC annotations (e.g. "4.3.2.5.7 4.5.2.34" into
      # "4.3.2.5.7","4.5.2.34")
      Annotation = list(unlist(strsplit(Annotation, " ")))
    ) %>%
    ungroup() %>%
    rename(Gene = 1, Genome = 2) # rename columns

  gene_count_table <- gene_count_table %>%
    as_tibble() %>%
    filter(.[[1]] %in% intersect) %>% # filter overlapping genes
    rename(Gene = 1) # rename gene column

  # Merge annotations and count data
  annotation_gene_count_table <- annotation_table %>%
    inner_join(gene_count_table, by = "Gene")

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
      filter(!purrr::map_lgl(Annotation, is.null)) %>% # remove genes without annotations
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
# nolint end


# compute_qgift ----

# nolint start
#' Calculates the quantitative GIFTs from a gene bundle and a gene count table
#'
#' @param definition Definition string of a given gene bundle
#' @param gene_count_table Table containing quantitative data of genes (e.g.,
#' expression or abundance)
#' @return A list of quantitative GIFT values
#' @examples
#' \dontrun{
#' compute_qGIFT(definition, gene_count_table)
#' }
#' @noexport

# UNDER DEVELOPMENT
compute_qgift <- function(definition, gene_count_table) {
  # If using EC codes, modify them to allow calculations
  if (grepl(".", definition, fixed = TRUE)) {
    gene_count_table <- gene_count_table %>%
      rowwise() %>%
      mutate(
        Annotation =
          list(purrr::map_chr(
            Annotation,
            function(x) stringr::str_replace_all(x, "\\.", "_")
          ))
      ) %>%
      ungroup()

    definition <- gsub(".", "_", definition, fixed = TRUE)
  }

  # Declare definition table
  definition_quantitative_list <-
    as.list(rep(definition, ncol(gene_count_table) - 1))
  names(definition_quantitative_list) <- colnames(gene_count_table)[-1]

  # Decompose definition
  def_decomp_list <- lapply(
    definition_quantitative_list,
    function(x) unlist(strsplit(x, "(?=[ ( ),+]+)", perl = TRUE))
  )

  # Set levels
  def_level_list <- lapply(def_decomp_list, function(x) set_levels(x))

  # Definition-level table
  def_table_list <- list()
  for (l in names(def_level_list)) {
    def_table <- create_step_matrix(def_decomp_list[[l]], def_level_list[[l]])
    def_table_list[[l]] <- def_table
  }

  # Calculate number of levels
  levels_list <- lapply(def_table_list, function(x) colnames(x)[c(3:ncol(x))])

  # Iterate calculation across levels and samples
  for (level in rev(levels_list[[1]])) {
    for (s in names(definition_quantitative_list)) {
      definition_quantitative_list[[s]] <-
        distillq_definition(
          sample = s,
          definition_expression = definition_quantitative_list[[s]],
          def_table_list[[s]],
          level = level,
          gene_count_table = gene_count_table %>% select(Annotation, s)
        )
      if (level != "L0_group") {
        def_decomp_list[[s]] <- unlist(strsplit(
          x = definition_quantitative_list[[s]],
          split = "(?=[ ( ),+]+)",
          perl = TRUE
        ))
        def_level_list[[s]] <- set_levels(def_decomp_list[[s]])
        def_table_list[[s]] <- create_step_matrix(
          def_decomp = def_decomp_list[[s]],
          def_level = def_level_list[[s]]
        )
      }
    }
  }
  # Return value
  return(definition_quantitative_list)
}
# nolint end


# sweep_matrix_list ----

#' Sweeps list names and row names in a list of matrices
#'
#' @param matrix_list A list of matrices (e.g. qGIFT_table yielded by
#' distillq())
#'
#' @return  A list of matrices with sweeped list names and row names
#'
#' @examples
#' \dontrun{
#' sweep_matrix_list(matrix_list)
#' }
#' @noexport

sweep_matrix_list <- function(matrix_list) {
  # Declare vectors
  rows <- rownames(matrix_list[[1]])
  columns <- colnames(matrix_list[[1]])
  tables <- names(matrix_list)

  # Create new empty list of matrices
  newlist <- replicate(
    n = length(rows),
    expr = matrix(NA, length(tables), length(columns)),
    simplify = FALSE
  )
  names(newlist) <- rows

  # Populate empty list of matrices
  for (row in rows) {
    for (table in tables) {
      colnames(newlist[[row]]) <- columns
      rownames(newlist[[row]]) <- tables
      newlist[[row]][table, ] <- matrix_list[[table]][row, ]
    }
  }

  # Return object
  return(newlist)
}


# distillq_definition ----
# nolint start
#' Generates the scores of each hierarchical level of a gene bundle required to calculate gene count-based GIFTs
#'
#' @param sample Code of the sample to be processed
#' @param definition_expression Definition-expression string
#' @param def_table Decomposed hierarchy matrix produced by create_step_matrix.R
#' @param level Hierarchical level of the pathway definition
#' @param gene_count_table Table containing quantitative data of genes (e.g., expression or abundance)
#' @importFrom stringr str_sub str_detect
#' @return A (partially) distilled definition string
#' @examples
#' \dontrun{
#' distill_definition_expression(definition_expression, def_table, level, gene_count_table)
#' }
#' @noexport

distillq_definition <- function(sample, definition_expression, def_table, level, gene_count_table) {
  FindID <- function(x, idToFind) {
    any(idToFind %in% x)
  }

  if (level == "L0_group") {
    def_table$clusters <- 0
    def_table_sub <- def_table[stats::complete.cases(def_table[, level]), ]
    clusters <- unique(def_table_sub$clusters)
  } else if (level == "L1_group") {
    def_table$clusters <- def_table[, "L0_group"]
    def_table_sub <- def_table[stats::complete.cases(def_table[, level]), ]
    clusters <- unique(def_table_sub$clusters)
  } else {
    def_table$clusters <- do.call(paste, c(def_table[, c(3:(ncol(def_table) - 1))], sep = "-"))
    def_table_sub <- def_table[stats::complete.cases(def_table[, level]), ]
    clusters <- unique(def_table_sub$clusters)
  }

  for (c in clusters) {
    subdef <- def_table_sub[def_table_sub$clusters == c, "def_decomp"]
    if (" " %in% subdef | "+" %in% subdef) {
      subdef2 <- subdef[(subdef != " ") & (subdef != "+")]
      subdef2_code <- subdef2[grepl("_", subdef2, fixed = TRUE) | grepl("[A-Z]", subdef2, fixed = FALSE)]
      subdef2_number <- as.numeric(subdef2[!subdef2 %in% subdef2_code])
      if (length(subdef2_code) > 0) {
        subdef2_expression <- gene_count_table %>%
          mutate(Flag = purrr::map_lgl(Annotation, FindID, subdef2_code)) %>%
          filter(Flag) %>%
          pull(sample)
        if (length(subdef2_expression) == 0) {
          subdef2_expression <- 0
        }
      } else {
        subdef2_expression <- NA
      }
      if (length(subdef2_expression) > 0 | is.na(subdef2_expression)) {
        value <- mean(c(subdef2_number, subdef2_expression), na.rm = TRUE)
      } else {
        value <- 0
      }
    } else if ("," %in% subdef) {
      subdef2 <- subdef[subdef != ","]
      subdef2_code <- subdef2[grepl("_", subdef2, fixed = TRUE) | grepl("[A-Z]", subdef2, fixed = FALSE)]
      subdef2_number <- as.numeric(subdef2[!subdef2 %in% subdef2_code])
      if (length(subdef2_code) > 0) {
        subdef2_expression <- gene_count_table %>%
          mutate(Flag = purrr::map_lgl(Annotation, FindID, subdef2_code)) %>%
          filter(Flag) %>%
          pull(sample)
        if (length(subdef2_expression) == 0) {
          subdef2_expression <- 0
        }
      } else {
        subdef2_expression <- NA
      }
      if (length(subdef2_expression) > 0 | is.na(subdef2_expression)) {
        value <- max(c(subdef2_number, subdef2_expression), na.rm = TRUE)
      } else {
        value <- 0
      }
    } else {
      subdef2 <- subdef
      subdef2_code <- subdef2[grepl("_", subdef2, fixed = TRUE) | grepl("[A-Z]", subdef2, fixed = FALSE)]
      subdef2_number <- as.numeric(subdef2[!subdef2 %in% subdef2_code])
      if (length(subdef2_code) > 0) {
        subdef2_expression <- gene_count_table %>%
          mutate(Flag = purrr::map_lgl(Annotation, FindID, subdef2_code)) %>%
          filter(Flag) %>%
          pull(sample)
        if (length(subdef2_expression) == 0) {
          subdef2_expression <- 0
        }
      } else {
        subdef2_expression <- NA
      }
      if (length(subdef2_expression) > 0 | is.na(subdef2_expression)) {
        value <- max(c(subdef2_number, subdef2_expression), na.rm = TRUE)
      } else {
        value <- 0
      }
    }
    if (level == "L0_group") {
      definition_expression <- gsub(paste(subdef, collapse = ""), value, definition_expression, fixed = TRUE)
    } else {
      definition_expression <- gsub(paste(c("(", subdef, ")"), collapse = ""), value, definition_expression, fixed = TRUE)
    }
  }

  return(definition_expression)
}
# nolint end
