#' Aggregates compound-level MCIs into function-level MCIs
#'
#' @param functions_table Function-level MCI table
#' @param pathway_db Table containing definitions and metadata of metabolic functions (provided by GAMMA)
#' @import tidyverse
#' @import dplyr
#' @return A table with Biosynthesis, Degradation and Overall MCI values
#' @examples
#' distill_total(functions_table,pathway_db)
#' @export


distill_total <- function(functions_table,pathway_db){

  MCI_total <- t(as.data.frame(t(functions_table)) %>%
    rownames_to_column('Code_function') %>%
    left_join(pathway_db[,c('Code_function', 'Type')], by = 'Code_function') %>%
    #Calculate Biosynthesis and Degradation MCIs
    group_by(Type) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    #Calculate overall MCI
    ungroup %>%
    summarise(Type = c(Type, 'Overall'), across(where(is.numeric), ~ c(., mean(.)))) %>%
    arrange(factor(Type, levels = unique(pathway_db$Type))) %>%
    column_to_rownames('Type'))

  return(MCI_total)
}
