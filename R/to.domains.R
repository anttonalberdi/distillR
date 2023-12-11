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

  #Convert tables into data frames
  if(!missing(functions_table)){functions_table <- as.data.frame(functions_table)}
  if(!missing(GIFT_db)){GIFT_db <- as.data.frame(GIFT_db)}

  domain_table <- t(as.data.frame(t(functions_table)) %>%
    rownames_to_column('Code_function') %>%
    left_join(unique(GIFT_db[,c('Code_function', 'Domain')]), by = 'Code_function') %>%
    #Calculate Biosynthesis and Degradation MCIs
    group_by(Domain) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    #Calculate overall MCI
    ungroup %>%
    arrange(factor(Domain, levels = unique(GIFT_db$Domain))) %>%
    column_to_rownames('Domain'))

  return(domain_table)
}
