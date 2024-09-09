# distill ----

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
#' \dontrun{
#' distill(
#'   distillR::gene_annotations,
#'   distillR::GIFT_db,
#'   genomecol = 2,
#'   annotcol = c(9, 10, 19)
#' )
#' }
#' @export

distill <- function(
    annotation_table,
    giftdb,
    genomecol = 2,
    annotcol = c(9, 10, 19)) {
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
    annotations_genome[, annotcol] %>% # annotcol is a number, not a name!
    c() %>%
    unlist() %>%
    c() %>%
    stringr::str_extract(
      pattern = "K[0-9]{5}|(?<=\\[EC:).+?(?=\\])"
    ) %>%
    unique() %>%
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
    giftdb) {
  gift_table <- c()
  n_genomes <- length(genomes)

  for (genome in genomes) {
    if (n_genomes > 1) {
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


# compute_gift ----

#' Calculates a GIFT from a gene bundle
#'
#' @param definition Definition string of a given gene bundle
#' @param present Vector of identifiers present in the genome
#' @return A GIFT value
#' @examples
#' compute_gift(
#'   "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))",
#'   c("K01580", "K00823", "K16871")
#' )
#' @export

compute_gift <- function(definition, present) {
  present <- sanitize_identifiers(present)
  definition <- sanitize_identifiers(definition)
  definition_decomposed <- decompose_definition(definition)
  definition_level <- set_levels(definition_decomposed)
  definition_table <-
    create_step_matrix(definition_decomposed, definition_level)
  levels <- colnames(definition_table)[c(3:ncol(definition_table))]

  for (level in rev(levels)) {
    definition <-
      distill_definition(definition, definition_table, level, present)
    if (level != "L0_group") {
      definition_decomposed <- decompose_definition(definition)
      definition_level <- set_levels(definition_decomposed)
      definition_table <-
        create_step_matrix(definition_decomposed, definition_level)
    }
  }

  return(as.numeric(definition))
}


# create_step_matrix ----
#' Creates a matrix that specifies the hierarchical structure of the definition
#' of a metabolic pathway/module
#'
#' @param def_decomp Vector of decomposed units of a definition string of a
#' metabolic pathway/module (produced by decompose_definition())
#' @param def_level Vector of the hierarchical level of decomposed units of a
#' definition string of a metabolic pathway/module (produced by set_levels())
#' @return A decomposed hierarchy matrix
#' @export

create_step_matrix <- function(def_decomp, def_level) {
  # Create table structure
  def_table <- as.data.frame(cbind(def_decomp, def_level))

  # Create level 0 structure
  level0_splits <- rownames(
    def_table[
      (def_decomp %in% c(" ", "+", ",")) & (def_level == 0),
    ]
  )
  level0_group <- c()
  group <- 1
  for (i in seq_along(def_decomp)) {
    if (i %in% level0_splits) {
      group <- group + 1
    }
    level0_group <- c(level0_group, group)
  }
  def_table[, "L0_group"] <- level0_group

  # If more hierarchical levels exist, iterate across levels.
  max_level <- max(def_level)
  if (max_level > 0) {
    levels <- c(1:max_level)
    # Next levels
    for (level in levels) {
      splits <-
        rownames(def_table[
          (def_decomp %in% c(" ", "+", ",")) & (def_level <= level),
        ])
      level_group <- c()
      group <- 1
      for (i in seq_along(def_decomp)) {
        if (i %in% splits) {
          group <- group + 1
        }
        if (def_table[i, "def_level"] >= level) {
          level_group <- c(level_group, group)
        } else {
          level_group <- c(level_group, NA)
          group <- 1
        }
      }
      def_table[, paste0("L", level, "_group")] <- level_group
    }
  }

  # Polish matrix
  def_table[
    def_table$def_decomp %in% c("(", ")"),
  ] <- NA

  # Print matrix
  return(def_table)
}


# decompose_definition ----
decompose_definition <- function(definition) {
  unlist(strsplit(definition, "(?=[ ( ),+]+)", perl = TRUE))
}


# distill_definition ----
#' Generates the scores of each hierarchical level of a gene bundle required to
#' calculate gene presence-based GIFTs
#'
#' @param definition Definition string of a given metabolic pathway/module
#' @param definition_table Decomposed hierarchy matrix produced by
#' create_step_matrix.R
#' @param level Hierarchical level
#' @param present Vector of gene identifiers present in the genome
#' @importFrom stringr str_sub
#' @return A (partially) distilled definition string
#' @examples
#' \dontrun{
#' distill_definition(definition, definition_table, level, present)
#' }
#' @export

distill_definition <- function(definition, definition_table, level, present) {
  definition_table <- process_clusters(definition_table, level)

  definition_table_sub <-
    definition_table[stats::complete.cases(definition_table[, level]), ]
  clusters <- unique(definition_table_sub$clusters)

  for (cluster in clusters) {
    subdef <-
      definition_table_sub %>%
      filter(clusters == cluster) %>%
      .$def_decomp

    value <- get_value(subdef = subdef, present = present)

    pattern <- ifelse(
      level == "L0_group",
      paste(subdef, collapse = ""),
      paste(c("(", subdef, ")"), collapse = "")
    )

    definition <- gsub(
      pattern = pattern,
      replacement = value %>% round(2),
      x = definition,
      fixed = TRUE
    )
  }

  return(definition)
}


#' Fill the defintion_table with cluster information depending on the level
#' we are analyzing
#' @param definition_table defintion table
#' @param level level being analyzed
#'
#' @return definition_table with the column clusters attached/overwritten
#' @noRd

process_clusters <- function(definition_table, level) {
  if (level == "L0_group") {
    definition_table$clusters <- 0
  } else if (level == "L1_group") {
    definition_table$clusters <- definition_table$L0_group
  } else {
    definition_table$clusters <-
      do.call(
        what = paste,
        args = c(
          definition_table[, c(3:(ncol(definition_table) - 1))],
          sep = "-"
        )
      )
  }
  return(definition_table)
}


#' Process subdef2 by checking which elements in present are in subdef2
#'
#' @param subdef2 subdefinition from distill_definition
#' @param present vector of present enzymes (KOs, ECs, peptidases)
#'
#' @return numeric vector of presences, absences and NAs
#' @noRd

process_subdef2 <- function(subdef2, present) {
  indexes <-
    grepl("_", subdef2, fixed = TRUE) |
      grepl("[A-Z]", subdef2, fixed = FALSE)
  subdef2[indexes] <- subdef2[indexes] %in% c(present)
  subdef2[subdef2 == "FALSE"] <- 0
  subdef2[subdef2 == "TRUE"] <- 1
  subdef2[subdef2 == ","] <- NA # Avoid warnings. May need more chars
  return(subdef2 %>% as.numeric())
}


#' Get the subdefinition score depending on the vector of present genes
#'
#' @param subdef subdefinition being processed
#' @param present present KO / EC / peptidase ids
#'
#' @return completeness of the subdefinition (0 <= value <= 1)
#'
#' @noRd

get_value <- function(subdef, present) {
  if (" " %in% subdef || "+" %in% subdef) {
    value <-
      subdef[(subdef != " ") & (subdef != "+")] %>%
      process_subdef2(present) %>%
      mean()
  } else {
    value <-
      subdef[subdef != ","] %>%
      process_subdef2(present) %>%
      max(0, .)
  }
  return(value)
}


# sanitize_identifiers ----
sanitize_identifiers <- function(definition) {
  definition %>% stringr::str_replace_all("\\.", "_")
}


# set_levels ----
#' Calculates the hierarchical levels of the definition of a metabolic
#' pathway/module
#'
#' @param definition_decomposed A decomposed definition string of a given
#' metabolic function (produced by decompose_definition())
#' @return A vector of hierarchical levels per symbol in definition_decomposed
#' @examples
#' \dontrun{
#' set_levels(definition_decomposed)
#' }
#' @export

set_levels <- function(definition_decomposed) {
  definition_levels <- definition_decomposed %>%
    length() %>%
    length()
  level <- 0
  for (position in seq_along(definition_decomposed)) {
    chr <- definition_decomposed[position]
    level <- level + (chr == "(") - (chr == ")")
    definition_levels[position] <- level
  }
  return(definition_levels)
}
