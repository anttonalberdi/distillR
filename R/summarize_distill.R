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
  domain_table <-
    functions_table %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Code_function") %>%
    left_join(gift_db[, c("Code_function", "Domain")], by = "Code_function") %>%
    group_by(Domain) %>% # nolint
    reframe(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    reframe(
      Domain = c(Domain, "Overall"),
      across(where(is.numeric), ~ c(., mean(.)))
    ) %>%
    arrange(factor(Domain, levels = unique(gift_db$Domain))) %>%
    column_to_rownames("Domain") %>%
    t()

  return(domain_table)
}


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
    group_by(Code_element) %>% # nolint
    summarise(across(where(is.numeric), ~ max(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_element, levels = unique(gift_db$Code_element))) %>%
    column_to_rownames("Code_element") %>%
    t()

  return(elements_table)
}


#' Aggregates element-level GIFTs into the function level
#'
#' @param elements_table Element-level GIFT table
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import dplyr
#' @return A GIFT table aggregated at the function level
#' @examples
#' \dontrun{
#' to.functions(elements_table, gift_db)
#' }
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
    group_by(Code_function) %>% # nolint
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    arrange(factor(Code_function, levels = unique(gift_db$Code_function))) %>%
    column_to_rownames("Code_function") %>%
    t()

  return(functions_table)
}



#' Converts genome-xpecific GIFTs into the community level
#'
#' @param gift_table GIFT table at any hierarchical level
#' @param abundance_table (Relative) abundance table with samples in columns
#' and Genomes in rows. Required for computing sample-specific GIFT values
#' @param gift_db Table containing definitions and metadata of GIFTs (default:
#' database provided by distillR)
#' @import dplyr
#' @return A matirx with community-level GIFTs per sample
#' @examples
#' \dontrun{
#' to.community(functions_table, abundance_table, gift_db)
#' }
#' @export


to_community <- function(gift_table, abundance_table, gift_db) {
  if (missing(abundance_table)) {
    # If abundance table does not exist, create a mock abundance table of a
    # single even community
    message(
      "\tWARNING: As no relative abundance information was provided distillR\n",
      "\twill generate a single community with evenly weighed genomes\n"
    )
    n <- nrow(gift_table)
    abundance_table <- rep(1 / n, n)
    rownames(abundance_table) <- rownames(gift_table)
    colnames(abundance_table) <- "GIFT"
    communities <- "GIFT"
  } else {
    communities <- colnames(abundance_table)
  }

  gift_community_table <- c()
  m <- 0
  for (community in communities) {
    m <- m + 1
    community_gift <- colSums(
      sweep(
        x = gift_table,
        MARGIN = 1,
        STATS = abundance_table[, m],
        FUN = "*",
        check.margin = FALSE
      )
    )
    gift_community_table <- rbind(gift_community_table, community_gift)
  }
  rownames(gift_community_table) <- communities

  return(gift_community_table)
}
