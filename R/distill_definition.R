#' Generates the scores of each hierarchical level of a gene bundle required to
#' calculate gene presence-based GIFTs
#'
#' @param definition Definition string of a given metabolic pathway/module
#' @param def_table Decomposed hierarchy matrix produced by create_step_matrix.R
#' @param level Hierarchical level
#' @param present Vector of gene identifiers present in the genome
#' @importFrom stringr str_sub
#' @return A (partially) distilled definition string
#' @examples
#' \dontrun{
#' distill_definition(definition, def_table, level, present)
#' }
#' @export


# UNDER DEVELOPMENT
distill_definition <- function(definition, def_table, level, present) {

  if (level == "L0_group") {
    def_table$clusters <- 0
  } else if (level == "L1_group") {
    def_table$clusters <- def_table[, "L0_group"]
  } else {
    def_table$clusters <-
      do.call(paste, c(def_table[, c(3:(ncol(def_table) - 1))], sep = "-"))
  }
  def_table_sub <- def_table[stats::complete.cases(def_table[, level]), ]
  clusters <- unique(def_table_sub$clusters)

  for (cluster in clusters) {

    subdef <- def_table_sub[def_table_sub$clusters == cluster, "def_decomp"]

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

    if (level == "L0_group") {
      pattern <- paste(subdef, collapse = "")
    } else {
      pattern <- paste(c("(", subdef, ")"), collapse = "")
    }

    definition <- gsub(
      pattern = pattern,
      replacement = value %>% round(2),
      x = definition,
      fixed = TRUE
    )
  }

  return(definition)
}


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
