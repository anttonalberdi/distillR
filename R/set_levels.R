#' Calculates the hierarchical levels of the definition of a metabolic
#' pathway/module
#'
#' @param definition_decomposed A decomposed definition string of a given
#' metabolic function (produced by decompose_definition())
#' @return A vector of hierarchical levels per character in string
#' @examples
#' set_levels(definition_decomposed)
#' @export

set_levels <- function(definition_decomposed) {
  definition_levels <- list()
  level <- 0
  for (i in seq_along(definition_decomposed)) {
    chr <- definition_decomposed[i]
    if (chr == "(") {
      level <- level + 1
    }
    if (chr == ")") {
      level <- level - 1
    }
    definition_levels[[i]] <- level
  }

  return(definition_levels %>% as.integer())
}
