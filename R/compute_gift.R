#' Calculates a GIFT from a gene bundle
#'
#' @param definition Definition string of a given gene bundle
#' @param present Vector of identifiers present in the genome
#' @return A GIFT value
#' @examples
#' compute_GIFT(definition, present)
#' compute_GIFT(
#'   "K01580 (K13524,K07250,K00823,K16871) (K00135,(K00139,K17761))",
#'   c("K01580", "K00823", "K16871")
#' )
#' @export

compute_gift <- function(definition, present) {

  present <- sanitize_identifiers(present)
  definition <- sanitize_identifiers(definition)
  definition_decomposed <- decompose_definition(definition)
  definition_level <- set_levels(definition_decomposed)
  definition_table <- create_step_matrix(definition_decomposed, definition_level)
  levels <- colnames(definition_table)[c(3:ncol(definition_table))]

  for (level in rev(levels)) {
    definition <- distill_definition(definition, definition_table, level, present)
    if (level != "L0_group") {
      definition_decomposed <- decompose_definition(definition)
      definition_level <- set_levels(definition_decomposed)
      definition_table <- create_step_matrix(definition_decomposed, definition_level)
    }
  }

  return(as.numeric(definition))
}
