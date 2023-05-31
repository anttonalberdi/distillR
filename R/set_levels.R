#' Calculates the hierarchical levels of the definition of a metabolic
#' pathway/module
#'
#' @param definition_decomposed A decomposed definition string of a given
#' metabolic function (produced by decompose_definition())
#' @return A vector of hierarchical levels per symbol in definition_decomposed
#' @examples
#' \dontrun{
#' set_levels(definition_decomposed)
#' }
#' @export

set_levels <- function(definition_decomposed) {
  definition_levels <- definition_decomposed %>% length() %>% length()
  level <- 0
  for (position in seq_along(definition_decomposed)) {
    chr <- definition_decomposed[position]
    level <- level + (chr == "(") - (chr == ")")
    definition_levels[position] <- level
  }
  return(definition_levels)
}
