#' Converts genome-xpecific GIFTs into the community level
#'
#' @param gift_table GIFT table at any hierarchical level
#' @param abundance_table (Relative) abundance table with samples in columns
#' and Genomes in rows. Required for computing sample-specific GIFT values
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import dplyr
#' @return A matirx with community-level GIFTs per sample
#' @examples
#' \dontrun{
#' to.community(functions_table, abundance_table, gift_db)
#' }
#' @export


to_community <- function(gift_table, abundance_table, gift_db) {

  if (missing(abundance_table)) {
    # If abundance table does not exist, create a mock abundance table of a
    # single even community
    message(
      "\tWARNING: As no relative abundance information was provided distillR\n",
      "\twill generate a single community with evenly weighed genomes\n"
      )
    n <- nrow(gift_table)
    abundance_table <- rep(1 / n, n)
    rownames(abundance_table) <- rownames(gift_table)
    colnames(abundance_table) <- "GIFT"
    communities <- "GIFT"
  } else {
    communities <- colnames(abundance_table)
  }

  gift_community_table <- c()
  m <- 0
  for (community in communities) {
    m <- m + 1
    community_gift <- colSums(
      sweep(
        x = gift_table,
        MARGIN = 1,
        STATS = abundance_table[, m],
        FUN = "*",
        check.margin = FALSE
      )
    )
    gift_community_table <- rbind(gift_community_table, community_gift)
  }
  rownames(gift_community_table) <- communities

  return(gift_community_table)
}
