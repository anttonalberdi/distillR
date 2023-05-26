#' Converts genome-xpecific GIFTs into the community level
#'
#' @param gift_table GIFT table at any hierarchical level
#' @param abundance_table (Relative) abundance table with samples in columns
#' and Genomes in rows. Required for computing sample-specific GIFT values
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A matirx with community-level GIFTs per sample
#' @examples
#' to.community(functions_table, abundance_table, gift_db)
#' @export


to.community <- function(gift_table, abundance_table, gift_db) {

  # Convert tables into data frames
  if (!missing(gift_table)) {
    gift_table <- as.data.frame(gift_table)
  }
  if (!missing(abundance_table)) {
    abundance_table <- as.data.frame(abundance_table)
  }
  if (!missing(gift_db)) {
    gift_db <- as.data.frame(gift_db)
  }

  if (missing(abundance_table)) {
    # If abundance table does not exist, create a mock abundance table of a single even community
    cat("\tWARNING: As no relative abundance information was provided distillR\n")
    cat("\twill generate a single community with evenly weighed genomes\n")
    abundance_table <- rep(1 / nrow(gift_table), nrow(gift_table))
    abundance_table <- t(t(abundance_table))
    rownames(abundance_table) <- rownames(gift_table)
    colnames(abundance_table) <- "GIFT"

    # Declare single community
    communities <- "GIFT"
  } else {
    # Declare communities from abundance table
    communities <- colnames(abundance_table)
  }

  GIFT_community_table <- c()
  m <- 0
  for (community in communities) {
    m <- m + 1
    community_GIFT <- colSums(sweep(gift_table, 1, abundance_table[, m], FUN = "*", check.margin = FALSE))
    GIFT_community_table <- rbind(GIFT_community_table, community_GIFT)
  }
  rownames(GIFT_community_table) <- communities

  return(GIFT_community_table)
}
