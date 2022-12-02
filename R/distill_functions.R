#' Aggregates compound-level MCIs into function-level MCIs
#'
#' @param compounds_table Compound-level MCI table
#' @param pathway_db Table containing definitions and metadata of metabolic functions (provided by GAMMA)
#' @import tidyverse
#' @import dplyr
#' @return A MCI table aggregated at the function level
#' @examples
#' distill_functions(compound_table)
#' @export


distill_functions <- function(compounds_table,pathway_db){

  functions_table <- t(as.data.frame(t(compounds_table)) %>%
    rownames_to_column('Code_compound') %>%
    left_join(pathway_db[,c('Code_compound', 'Code_function')], by = 'Code_compound') %>%
    group_by(Code_function) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_function, levels = unique(pathway_db$Code_function))) %>%
    column_to_rownames('Code_function'))

  return(functions_table)
}
