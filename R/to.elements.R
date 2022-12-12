#' Merges bundle-level GIFTs into the element level
#'
#' @param GIFT_table Bundle-level GIFT table
#' @param GIFT_db Table containing definitions and metadata of GIFTs (default: database provided by distillR)
#' @return A GIFT table aggregated at the element level
#' @import tidyverse
#' @import dplyr
#' @import tibble
#' @examples
#' to.elements(GIFT_table,GIFT_db)
#' @export

to.elements <- function(GIFT_table,GIFT_db){

  #Convert tables into data frames
  if(!missing(GIFT_table)){GIFT_table <- as.data.frame(GIFT_table)}
  if(!missing(GIFT_db)){GIFT_db <- as.data.frame(GIFT_db)}

   elements_table <- t(as.data.frame(t(GIFT_table)) %>%
     rownames_to_column('Code_bundle') %>%
     left_join(GIFT_db[,c('Code_bundle', 'Code_element')], by = 'Code_bundle') %>%
     group_by(Code_element) %>%
     summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
     arrange(factor(Code_element, levels = unique(GIFT_db$Code_element))) %>%
     column_to_rownames('Code_element'))

   return(elements_table)
}
