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
  subdef2[subdef2 == ","] <- NA  # Avoid warnings. May need more chars
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
}
