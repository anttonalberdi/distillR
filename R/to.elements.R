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

   elements_table <- t(as.data.frame(t(GIFT_table)) %>%
     rownames_to_column('Code_pathway') %>%
     left_join(GIFT_db[,c('Code_pathway', 'Code_compound')], by = 'Code_pathway') %>%
     group_by(Code_compound) %>%
     summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
     arrange(factor(Code_compound, levels = unique(GIFT_db$Code_compound))) %>%
     column_to_rownames('Code_compound'))

   return(elements_table)
}
