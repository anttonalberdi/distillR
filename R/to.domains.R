#' Aggregates function-level GIFTs into the domain level
#'
#' @param functions_table Function-level GIFT table
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A table with Biosynthesis, Degradation and Overall MCI values
#' @examples
#' to.domains(functions_table,GIFT_db)
#' @export


to.domains <- function(functions_table,GIFT_db){

  domain_table <- t(as.data.frame(t(functions_table)) %>%
    rownames_to_column('Code_function') %>%
    left_join(GIFT_db[,c('Code_function', 'Type')], by = 'Code_function') %>%
    #Calculate Biosynthesis and Degradation MCIs
    group_by(Type) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    #Calculate overall MCI
    ungroup %>%
    summarise(Type = c(Type, 'Overall'), across(where(is.numeric), ~ c(., mean(.)))) %>%
    arrange(factor(Type, levels = unique(GIFT_db$Type))) %>%
    column_to_rownames('Type'))

  return(domain_table)
}
