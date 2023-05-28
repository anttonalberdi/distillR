#' Aggregates element-level GIFTs into the function level
#'
#' @param elements_table Element-level GIFT table
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import tidyverse
#' @import dplyr
#' @return A GIFT table aggregated at the function level
#' @examples
#' to.functions(elements_table, gift_db)
#' @export


to_functions <- function(elements_table, gift_db) {

  functions_table <-
    elements_table %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Code_element") %>%
    left_join(
      gift_db[, c("Code_element", "Code_function")],
      by = "Code_element"
    ) %>%
    group_by(Code_function) %>%  # nolint
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_function, levels = unique(gift_db$Code_function))) %>%
    column_to_rownames("Code_function") %>%
    t()

  return(functions_table)
}
