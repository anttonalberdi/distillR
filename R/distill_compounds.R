#' Aggregates pathway-level MCIs into compound-level MCIs
#'
#' @param MCI_table Patheway-level MCI table
#' @param pathway_db Table containing definitions and metadata of metabolic functions (provided by GAMMA)
#' @return A MCI table aggregated at the compound level
#' @import tidyverse
#' @import dplyr
#' @import tibble
#' @examples
#' distill_compounds(compound_table,pathway_table)
#' @export

distill_compounds <- function(MCI_table,pathway_db){

   compounds_table <- t(as.data.frame(t(MCI_table)) %>%
     rownames_to_column('Code_pathway') %>%
     left_join(pathway_db[,c('Code_pathway', 'Code_compound')], by = 'Code_pathway') %>%
     group_by(Code_compound) %>%
     summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
     arrange(factor(Code_compound, levels = unique(pathway_db$Code_compound))) %>%
     column_to_rownames('Code_compound'))

   return(compounds_table)
}
