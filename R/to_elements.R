#' Merges bundle-level GIFTs into the element level
#'
#' @param gift_table Bundle-level GIFT table
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @return A GIFT table aggregated at the element level
#' @import dplyr
#' @import tibble
#' @examples
#' \dontrun{
#' to.elements(gift_table, gift_db)
#' }
#' @export

to_elements <- function(gift_table, gift_db) {

  elements_table <-
    gift_table %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Code_bundle") %>%
    left_join(
      gift_db[, c("Code_bundle", "Code_element")],
      by = "Code_bundle"
    ) %>%
    group_by(Code_element) %>%  # nolint
    summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_element, levels = unique(gift_db$Code_element))) %>%
    column_to_rownames("Code_element") %>%
    t()

  return(elements_table)
}
