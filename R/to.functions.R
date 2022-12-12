#' Aggregates element-level GIFTs into the function level
#'
#' @param elements_table Element-level GIFT table
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A GIFT table aggregated at the function level
#' @examples
#' to.functions(elements_table,GIFT_db)
#' @export


to.functions <- function(elements_table,GIFT_db){

  #Convert tables into data frames
  if(!missing(elements_table)){elements_table <- as.data.frame(elements_table)}
  if(!missing(GIFT_db)){GIFT_db <- as.data.frame(GIFT_db)}

  functions_table <- t(as.data.frame(t(elements_table)) %>%
    rownames_to_column('Code_compound') %>%
    left_join(GIFT_db[,c('Code_compound', 'Code_function')], by = 'Code_compound') %>%
    group_by(Code_function) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_function, levels = unique(GIFT_db$Code_function))) %>%
    column_to_rownames('Code_function'))

  return(functions_table)
}
