#' Aggregates function-level GIFTs into the domain level
#'
#' @param functions_table Function-level GIFT table
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import dplyr
#' @return A table with Biosynthesis, Degradation and Overall MCI values
#' @examples
#' \dontrun{
#' to.domains(functions_table, gift_db)
#' }
#' @export


to_domains <- function(functions_table, gift_db) {

  domain_table <- t(as.data.frame(t(functions_table)) %>%
    rownames_to_column("Code_function") %>%
    left_join(gift_db[, c("Code_function", "Domain")], by = "Code_function") %>%
    group_by(Domain) %>%  # nolint
    reframe(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    reframe(
      Domain = c(Domain, "Overall"),
      across(where(is.numeric), ~ c(., mean(.)))
    ) %>%
    arrange(factor(Domain, levels = unique(gift_db$Domain))) %>%
    column_to_rownames("Domain"))

  return(domain_table)
}
